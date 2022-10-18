use crate::ir::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

#[derive(Clone)]
enum Public { Yes, No }

#[derive(Clone)]
struct ExtraFn {
    f: LLVMValueRef,
    ftype: LLVMTypeRef,

    // public means that the user is allowed to call this function using a NativeFn.
    public: Public,
}

struct Ctxt {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    value_type: LLVMTypeRef,
    void_type: LLVMTypeRef,
    f64_type: LLVMTypeRef,
    bb: LLVMBasicBlockRef,

    nodes: HashMap<Node, LLVMValueRef>,

    // functions defined in extra/
    extra_fns: HashMap<String, ExtraFn>,
}

pub fn compile(ir: &IR) {
    unsafe {
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
        let builder = LLVMCreateBuilderInContext(context);
        let f64_type = LLVMDoubleTypeInContext(context);

        let void_type = LLVMVoidType();
        let value_type = {
            let uint64t = LLVMInt64TypeInContext(context);
            let mut elements = [uint64t, uint64t];

            LLVMStructTypeInContext(context, elements.as_mut_ptr(), elements.len() as u32, 0)
        };
        let bb = 0 as *mut _;

        let nodes = HashMap::new();

        let mut extra_fns = HashMap::new();

        let mut ctxt = Ctxt {
            context,
            module,
            builder,
            value_type,
            void_type,
            f64_type,
            bb,
            nodes,
            extra_fns,
        };

        // private:
        declare_extra_fn("new_table", value_type, &[], Public::No, &mut ctxt);
        declare_extra_fn("table_set", void_type, &[value_type, value_type, value_type], Public::No, &mut ctxt);
        declare_extra_fn("table_get", value_type, &[value_type, value_type], Public::No, &mut ctxt);
        declare_extra_fn("num", value_type, &[f64_type], Public::No, &mut ctxt);
        declare_extra_fn("nil", value_type, &[], Public::No, &mut ctxt);

        // public:
        declare_extra_fn("print", value_type, &[value_type], Public::Yes, &mut ctxt);

        compile_mainfn(&ir.fns[ir.main_fn], ir, &mut ctxt);

        LLVMDumpModule(ctxt.module);
    }
}

fn declare_extra_fn(fname: &str, ret: LLVMTypeRef, args: &[LLVMTypeRef], public: Public, ctxt: &mut Ctxt) {
    unsafe {
        let mut args: Vec<LLVMTypeRef> = args.iter().cloned().collect();
        let ftype = LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0);
        let name = format!("{}\0", fname);
        let f = LLVMAddFunction(ctxt.module, name.as_bytes().as_ptr() as *const _, ftype);
        let extra_fn = ExtraFn { f, ftype, public };
        ctxt.extra_fns.insert(fname.to_string(), extra_fn);
    }
}

fn call_extra_fn(fname: &str, args: &[LLVMValueRef], ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let extra_fn = ctxt.extra_fns[fname].clone();
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
    call_extra_fn("nil", &[], ctxt)
}

fn compile_expr(e: &Expr, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        match e {
            Expr::Num(x) => {
                let x = LLVMConstReal(ctxt.f64_type, *x);
                call_extra_fn("num", &[x], ctxt)
            },
            Expr::Nil => nil(ctxt),
            Expr::NewTable => {
                call_extra_fn("new_table", &[], ctxt)
            }
            Expr::NativeFn(s) => {
                println!("ignoring Expr::NativeFn");

                nil(ctxt)
            },
            Expr::Index(t, i) => {
                let args = [ctxt.nodes[t], ctxt.nodes[i]];
                call_extra_fn("table_get", &args, ctxt)
            },
            Expr::FnCall(_, arg) => {
                println!("assuming FnCall is print");

                let args = [ctxt.nodes[arg]];

                call_extra_fn("print", &args, ctxt)
            }
            _ => {
                println!("ignoring other Expr!");

                nil(ctxt)
            },
        }
    }
}

fn compile_mainfn(f: &LitFunction, ir: &IR, ctxt: &mut Ctxt) {
    unsafe {
        // create main
        let main_function_type = LLVMFunctionType(ctxt.void_type, [].as_mut_ptr(), 0, 0);
        let main_function = LLVMAddFunction(ctxt.module, b"main\0".as_ptr() as *const _, main_function_type);

        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.context, main_function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        for st in &f.body {
            match st {
                Statement::Compute(n, e) => {
                    let vref = compile_expr(e, ctxt);
                    ctxt.nodes.insert(*n, vref);
                },
                Statement::Store(t, i, v) => {
                    let args = [ctxt.nodes[t], ctxt.nodes[i], ctxt.nodes[v]];
                    call_extra_fn("table_set", &args, ctxt);
                },
                _ => {/* TODO */},
            }
        }

        LLVMBuildRetVoid(ctxt.builder);
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
