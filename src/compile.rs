use crate::ir::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

#[allow(non_camel_case_types)]
#[allow(dead_code)]
enum Tag {
    TABLE_PTR = 0,
    FN = 1,
    NIL = 2,
    NUM = 3,
    STR = 4,
    BOOL = 5,
}

#[derive(Clone)]
struct ExtraFn {
    f: LLVMValueRef,
    ftype: LLVMTypeRef,
}

struct Ctxt {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    value_type: LLVMTypeRef,
    void_type: LLVMTypeRef,
    f64_type: LLVMTypeRef,
    u64_type: LLVMTypeRef,
    v2v_ftype: LLVMTypeRef,
    v2v_fptr_type: LLVMTypeRef,
    bb: LLVMBasicBlockRef,

    nodes: HashMap<Node, LLVMValueRef>,

    // functions defined in extra/
    extra_fns: HashMap<String, ExtraFn>,

    lit_fns: HashMap<FnId, LLVMValueRef>,

    start_fn: LLVMValueRef,
}

pub fn compile(ir: &IR) {
    unsafe {
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
        let builder = LLVMCreateBuilderInContext(context);
        let f64_type = LLVMDoubleTypeInContext(context);
        let u64_type = LLVMInt64TypeInContext(context);

        let void_type = LLVMVoidType();
        let value_type = {
            let mut elements = [u64_type, u64_type];

            LLVMStructTypeInContext(context, elements.as_mut_ptr(), elements.len() as u32, 0)
        };

        let v2v_ftype = {
            let mut args = [value_type];
            LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as _, 0)
        };
        let v2v_fptr_type = LLVMPointerType(v2v_ftype, 0);

        let start_fn = {
            let mut args = [];
            let start_function_type = LLVMFunctionType(void_type, args.as_mut_ptr(), args.len() as _, 0);

            LLVMAddFunction(module, b"main\0".as_ptr() as *const _, start_function_type)
        };
        let lit_fns = HashMap::new();

        let bb = 0 as *mut _;

        let nodes = HashMap::new();

        let extra_fns = HashMap::new();

        let mut ctxt = Ctxt {
            context,
            module,
            builder,
            value_type,
            void_type,
            f64_type,
            u64_type,
            v2v_ftype,
            v2v_fptr_type,
            bb,
            nodes,
            extra_fns,
            start_fn,
            lit_fns,
        };

        // table implementation:
        declare_extra_fn("new_table", value_type, &[], &mut ctxt);
        declare_extra_fn("table_set", void_type, &[value_type, value_type, value_type], &mut ctxt);
        declare_extra_fn("table_get", value_type, &[value_type, value_type], &mut ctxt);

        // native functions:
        declare_extra_fn("print", value_type, &[value_type], &mut ctxt);
        declare_extra_fn("next", value_type, &[value_type], &mut ctxt);
        declare_extra_fn("pairs", value_type, &[value_type], &mut ctxt);
        declare_extra_fn("type", value_type, &[value_type], &mut ctxt);

        // declare lit fns
        for (fid, _) in ir.fns.iter().enumerate() {
            let name = format!("f{}\0", fid);
            let function = LLVMAddFunction(module, name.as_bytes().as_ptr() as *const _, v2v_ftype);
            ctxt.lit_fns.insert(fid, function);
        }

        compile_start_fn(ir.main_fn, &mut ctxt);

        // compile lit fns
        for (fid, lit_f) in ir.fns.iter().enumerate() {
            compile_fn(ctxt.lit_fns[&fid], lit_f, &mut ctxt);
        }

        LLVMDumpModule(ctxt.module);
    }
}

fn declare_extra_fn(fname: &str, ret: LLVMTypeRef, args: &[LLVMTypeRef], ctxt: &mut Ctxt) {
    unsafe {
        let mut args: Vec<LLVMTypeRef> = args.iter().cloned().collect();
        let ftype = LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0);
        let name = format!("{}\0", fname);
        let f = LLVMAddFunction(ctxt.module, name.as_bytes().as_ptr() as *const _, ftype);
        let extra_fn = ExtraFn { f, ftype };
        ctxt.extra_fns.insert(fname.to_string(), extra_fn);
    }
}

fn call_extra_fn(fname: &str, args: &[LLVMValueRef], ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let extra_fn: &ExtraFn = ctxt.extra_fns.get(fname).unwrap_or_else(|| panic!("unknown extra fn \"{}\"", fname));
        let mut args: Vec<LLVMValueRef> = args.iter().cloned().collect();

        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ extra_fn.ftype,
            /*Fn: */ extra_fn.f,
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        )
    }
}

fn nil(ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let i32t = LLVMInt32TypeInContext(ctxt.context);

        let v = LLVMBuildAlloca(ctxt.builder, ctxt.value_type, EMPTY);
        let zero = LLVMConstInt(i32t, 0, 0);
        let mut indices = [zero, zero];
        let ep = LLVMBuildGEP2(ctxt.builder, ctxt.value_type, v, indices.as_mut_ptr(), indices.len() as u32, EMPTY);
        LLVMBuildStore(ctxt.builder, LLVMConstInt(ctxt.u64_type, Tag::NIL as u64, 0), ep);
        LLVMBuildLoad2(ctxt.builder, ctxt.value_type, v, EMPTY)
    }
}

fn num(x: f64, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let i32t = LLVMInt32TypeInContext(ctxt.context);

        let v = LLVMBuildAlloca(ctxt.builder, ctxt.value_type, EMPTY);
        let zero = LLVMConstInt(i32t, 0, 0);
        let one = LLVMConstInt(i32t, 1, 0);

        let mut indices = [zero, zero];
        let ep = LLVMBuildGEP2(ctxt.builder, ctxt.value_type, v, indices.as_mut_ptr(), indices.len() as u32, EMPTY);
        LLVMBuildStore(ctxt.builder, LLVMConstInt(ctxt.u64_type, Tag::NUM as u64, 0), ep);

        let mut indices = [zero, one];
        let ep = LLVMBuildGEP2(ctxt.builder, ctxt.value_type, v, indices.as_mut_ptr(), indices.len() as u32, EMPTY);
        let f64ptr = LLVMPointerType(ctxt.f64_type, 0);
        let ep = LLVMBuildBitCast(ctxt.builder, ep, f64ptr, EMPTY);
        let real = LLVMConstReal(ctxt.f64_type, x);
        LLVMBuildStore(ctxt.builder, real, ep);

        LLVMBuildLoad2(ctxt.builder, ctxt.value_type, v, EMPTY)
    }
}

fn fn_call(f_val: LLVMValueRef, arg: LLVMValueRef, ctxt: &mut Ctxt) -> LLVMValueRef {
    // TODO add check that f is actually a function
    unsafe {
        let f /* i64 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 1, EMPTY);
        let f /* Value (*fn)(Value) */ = LLVMBuildIntToPtr(ctxt.builder, f, ctxt.v2v_fptr_type, EMPTY);

        let mut fargs = [arg];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2v_ftype,
            /*Fn: */ f,
            /*Args: */ fargs.as_mut_ptr(),
            /*Num Args: */ fargs.len() as u32,
            /*Name: */ EMPTY,
        )
    }
}


// f should be generated using LLVMAddFunction.
fn make_fn_value(f: LLVMValueRef, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let i32t = LLVMInt32TypeInContext(ctxt.context);

        let v = LLVMBuildAlloca(ctxt.builder, ctxt.value_type, EMPTY);
        let zero = LLVMConstInt(i32t, 0, 0);
        let one = LLVMConstInt(i32t, 1, 0);

        let mut indices = [zero, zero];
        let ep = LLVMBuildGEP2(ctxt.builder, ctxt.value_type, v, indices.as_mut_ptr(), indices.len() as u32, EMPTY);
        LLVMBuildStore(ctxt.builder, LLVMConstInt(ctxt.u64_type, Tag::FN as u64, 0), ep);

        let mut indices = [zero, one];
        let ep = LLVMBuildGEP2(ctxt.builder, ctxt.value_type, v, indices.as_mut_ptr(), indices.len() as u32, EMPTY);
        let ftype = LLVMPointerType(ctxt.v2v_fptr_type, 0);
        let ep = LLVMBuildBitCast(ctxt.builder, ep, ftype, EMPTY);
        LLVMBuildStore(ctxt.builder, f, ep);

        LLVMBuildLoad2(ctxt.builder, ctxt.value_type, v, EMPTY)
    }
}

fn lookup_native_fn(i: NativeFnId, ctxt: &mut Ctxt) -> LLVMValueRef {
    let s = NATIVE_FNS[i];
    let f = ctxt.extra_fns[s].f;
    make_fn_value(f, ctxt)
}

fn compile_expr(e: &Expr, ctxt: &mut Ctxt) -> LLVMValueRef {
    match e {
        Expr::Nil => nil(ctxt),
        Expr::Num(x) => num(*x, ctxt),
        Expr::NewTable => {
            call_extra_fn("new_table", &[], ctxt)
        }
        Expr::NativeFn(i) => lookup_native_fn(*i, ctxt),
        Expr::LitFunction(fid, _upnodes) => make_fn_value(ctxt.lit_fns[fid], ctxt),
        Expr::Index(t, i) => {
            let args = [ctxt.nodes[t], ctxt.nodes[i]];
            call_extra_fn("table_get", &args, ctxt)
        },
        Expr::FnCall(f, arg) => {
            let f = ctxt.nodes[f];
            let arg = ctxt.nodes[arg];

            fn_call(f, arg, ctxt)
        }
        x => {
            println!("ignoring other Expr {:?}!", x);

            nil(ctxt)
        },
    }
}

fn compile_start_fn(main_fn: FnId, ctxt: &mut Ctxt) {
    unsafe {
        let start_fn = ctxt.start_fn;

        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.context, start_fn, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        let v = LLVMBuildAlloca(ctxt.builder, ctxt.value_type, EMPTY);
        let v = LLVMBuildLoad2(ctxt.builder, ctxt.value_type, v, EMPTY);

        let mut args = [v];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2v_ftype,
            /*Fn: */ ctxt.lit_fns[&main_fn],
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        );

        LLVMBuildRetVoid(ctxt.builder);
    }
}

fn compile_fn(val_f: LLVMValueRef, lit_f: &LitFunction, ctxt: &mut Ctxt) {
    unsafe {
        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.context, val_f, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        for st in &lit_f.body {
            match st {
                Statement::Compute(n, e) => {
                    let vref = compile_expr(e, ctxt);
                    ctxt.nodes.insert(*n, vref);
                },
                Statement::Store(t, i, v) => {
                    let args = [ctxt.nodes[t], ctxt.nodes[i], ctxt.nodes[v]];
                    call_extra_fn("table_set", &args, ctxt);
                },
                Statement::Return(v) => {
                    let v = ctxt.nodes[v];
                    LLVMBuildRet(ctxt.builder, v);
                },
                _ => {/* TODO */},
            }
        }
    }

}

impl Drop for Ctxt {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.context);
        }
    }
}
