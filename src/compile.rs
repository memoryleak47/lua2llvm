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
    bb: LLVMBasicBlockRef,

    nodes: HashMap<Node, LLVMValueRef>,

    // functions defined in extra/
    extra_fns: HashMap<String, ExtraFn>,

    lit_fns: HashMap<FnId, LLVMValueRef>,

    start_fn: LLVMValueRef,
}

impl Ctxt {
    fn i32_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(self.context) }
    }

    fn i64_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMInt64TypeInContext(self.context) }
    }

    fn void_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMVoidType() }
    }

    fn f64_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMDoubleTypeInContext(self.context) }
    }

    fn value_t(&mut self) -> LLVMTypeRef {
        let mut elements = [self.i32_t(), self.i32_t(), self.i64_t()];
        unsafe { LLVMStructTypeInContext(self.context, elements.as_mut_ptr(), elements.len() as u32, 0) }
    }

    fn value_ptr_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.value_t(), 0) }
    }

    fn v2v_t(&mut self) -> LLVMTypeRef {
        let mut args = [self.value_ptr_t(), self.i32_t(), self.value_ptr_t()];
        unsafe { LLVMFunctionType(self.void_t(), args.as_mut_ptr(), args.len() as _, 0) }
    }

    fn v2v_ptr_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.v2v_t(), 0) }
    }
}

pub fn compile(ir: &IR) {
    unsafe {
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
        let builder = LLVMCreateBuilderInContext(context);

        let start_fn = {
            let mut args = [];
            let start_function_type = LLVMFunctionType(LLVMVoidType(), args.as_mut_ptr(), args.len() as _, 0);

            LLVMAddFunction(module, b"main\0".as_ptr() as *const _, start_function_type)
        };

        let bb = 0 as *mut _;

        let mut ctxt = Ctxt {
            context,
            module,
            builder,
            bb,
            nodes: Default::default(),
            extra_fns: Default::default(),
            start_fn,
            lit_fns: Default::default(),
        };

        // table implementation:
        declare_extra_fn("new_table", &[ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("table_set", &[ctxt.value_ptr_t(); 3], &mut ctxt);
        declare_extra_fn("table_get", &[ctxt.value_ptr_t(); 3], &mut ctxt);

        // native functions:
        for fname in NATIVE_FNS {
            declare_extra_fn(fname, &[ctxt.value_ptr_t(), ctxt.i32_t(), ctxt.value_ptr_t()], &mut ctxt);
        }

        // declare lit fns
        for fid in 0..ir.fns.len() {
            let name = format!("f{}\0", fid);
            let function = LLVMAddFunction(module, name.as_bytes().as_ptr() as *const _, ctxt.v2v_t());
            ctxt.lit_fns.insert(fid, function);
        }

        compile_start_fn(ir.main_fn, &mut ctxt);

        // compile lit fns
        for fid in 0..ir.fns.len() {
            compile_fn(ctxt.lit_fns[&fid], fid, ir, &mut ctxt);
        }

        LLVMDumpModule(ctxt.module);
    }
}

fn declare_extra_fn(fname: &str, args: &[LLVMTypeRef], ctxt: &mut Ctxt) {
    unsafe {
        let mut args: Vec<LLVMTypeRef> = args.iter().cloned().collect();
        let ftype = LLVMFunctionType(ctxt.void_t(), args.as_mut_ptr(), args.len() as u32, 0);
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
        let mut vals = [
            LLVMConstInt(ctxt.i32_t(), Tag::NIL as _, 0),
            LLVMGetUndef(ctxt.i32_t()),
            LLVMGetUndef(ctxt.i64_t())
        ];
        LLVMConstStructInContext(ctxt.context, vals.as_mut_ptr(), vals.len() as _, 0)
    }
}

fn num(x: f64, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let val = LLVMConstReal(ctxt.f64_t(), x);
        let val = LLVMBuildBitCast(ctxt.builder, val, ctxt.i64_t(), EMPTY);

        let mut vals = [
            LLVMConstInt(ctxt.i32_t(), Tag::NUM as _, 0),
            LLVMGetUndef(ctxt.i32_t()),
            val
        ];
        LLVMConstStructInContext(ctxt.context, vals.as_mut_ptr(), vals.len() as _, 0)
    }
}

fn fn_call(f_val: LLVMValueRef /* Value with FN tag */, arg: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value */ {
    // TODO add check that f is actually a function
    unsafe {
        let upvalue_stack_index /* i32 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 1, EMPTY);
        let f /* i64 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 2, EMPTY);
        let f /* Value (*fn)(Value) */ = LLVMBuildIntToPtr(ctxt.builder, f, ctxt.v2v_ptr_t(), EMPTY);

        let out = alloc(ctxt);
        let mut fargs = [alloc_val(arg, ctxt), upvalue_stack_index, out];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2v_t(),
            /*Fn: */ f,
            /*Args: */ fargs.as_mut_ptr(),
            /*Num Args: */ fargs.len() as u32,
            /*Name: */ EMPTY,
        );

        load_val(out, ctxt)
    }
}


// f should be generated using LLVMAddFunction of type v2v_t.
// upvalue_stack_index needs to be 0 for 0-closure functions.
fn make_fn_value(f: LLVMValueRef, upvalue_stack_index: i32, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let val = LLVMBuildPtrToInt(ctxt.builder, f, ctxt.i64_t(), EMPTY);

        let mut vals = [
            LLVMConstInt(ctxt.i32_t(), Tag::FN as _, 0),
            LLVMConstInt(ctxt.i32_t(), upvalue_stack_index as _, 0),
            val
        ];
        LLVMConstStructInContext(ctxt.context, vals.as_mut_ptr(), vals.len() as _, 0)
    }
}

fn mk_native_fn(i: NativeFnId, ctxt: &mut Ctxt) -> LLVMValueRef {
    let s = NATIVE_FNS[i];
    let f = ctxt.extra_fns[s].f;

    make_fn_value(f, 0, ctxt)
}

fn compile_expr(e: &Expr, current_fn: FnId, ctxt: &mut Ctxt) -> LLVMValueRef {
    match e {
        Expr::Nil => nil(ctxt),
        Expr::Num(x) => num(*x, ctxt),
        Expr::NewTable => {
            let var = alloc(ctxt);
            call_extra_fn("new_table", &[var], ctxt);

            load_val(var, ctxt)
        }
        Expr::NativeFn(i) => mk_native_fn(*i, ctxt),
        Expr::LitFunction(fid, _upnodes) => make_fn_value(ctxt.lit_fns[fid], 0, ctxt), // TODO upnodes
        Expr::Index(t, i) => {
            let t = alloc_val(ctxt.nodes[t], ctxt); // TODO this breaks!
            let i = alloc_val(ctxt.nodes[i], ctxt);
            let out = alloc(ctxt);
            call_extra_fn("table_get", &[t, i, out], ctxt);
            load_val(out, ctxt)
        },
        Expr::FnCall(f, arg) => {
            let f = ctxt.nodes[f];
            let arg = ctxt.nodes[arg];

            fn_call(f, arg, ctxt)
        }
        Expr::Arg => {
            unsafe {
                let param = LLVMGetParam(ctxt.lit_fns[&current_fn], 0);
                load_val(param, ctxt)
            }
        },
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

        let out_val = alloc(ctxt);
        let in_val = alloc_val(nil(ctxt), ctxt);

        let mut args = [
            in_val,
            LLVMConstInt(ctxt.i32_t(), 0, 0),
            out_val,
        ];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2v_t(),
            /*Fn: */ ctxt.lit_fns[&main_fn],
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        );

        LLVMBuildRetVoid(ctxt.builder);
    }
}

// will allocate a variable of type Value.
fn alloc(ctxt: &mut Ctxt) -> LLVMValueRef /* Value* */ {
    unsafe {
        LLVMBuildAlloca(ctxt.builder, ctxt.value_t(), EMPTY)
    }
}

// will allocate a variable pointing to the Value `x`.
fn alloc_val(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value* */ {
    unsafe {
        let var = alloc(ctxt);
        LLVMBuildStore(ctxt.builder, x, var);

        var
    }
}

// will load a Value*.
fn load_val(x: LLVMValueRef /* Value* */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value */ {
    unsafe {
        LLVMBuildLoad2(ctxt.builder, ctxt.value_t(), x, EMPTY)
    }
}

fn compile_fn(val_f: LLVMValueRef, fn_id: FnId, ir: &IR, ctxt: &mut Ctxt) {
    unsafe {
        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.context, val_f, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        let lit_f = &ir.fns[fn_id];
        for st in &lit_f.body {
            match st {
                Statement::Compute(n, e) => {
                    let vref = compile_expr(e, fn_id, ctxt);
                    ctxt.nodes.insert(*n, vref);
                },
                Statement::Store(t, i, v) => {
                    let t = alloc_val(ctxt.nodes[t], ctxt);
                    let i = alloc_val(ctxt.nodes[i], ctxt);
                    let v = alloc_val(ctxt.nodes[v], ctxt);
                    call_extra_fn("table_set", &[t, i, v], ctxt);
                },
                Statement::Return(v) => {
                    let v = ctxt.nodes[v];
                    let out = LLVMGetParam(val_f, 2);
                    LLVMBuildStore(ctxt.builder, v, out);
                    LLVMBuildRetVoid(ctxt.builder);
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
