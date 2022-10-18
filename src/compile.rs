use crate::ir::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

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
    extra_fns: HashMap<String, (/*fn: */ LLVMValueRef, /*fn type: */ LLVMTypeRef)>,
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

        // declare print
        {
            let mut args = [value_type];
            let ftype = LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as u32, 0);
            let f = LLVMAddFunction(module, b"print\0".as_ptr() as *const _, ftype);
            extra_fns.insert("print".to_string(), (f, ftype));
        }

        // declare table_set
        {
            let mut args = [value_type, value_type, value_type];
            let ftype = LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as u32, 0);
            let f = LLVMAddFunction(module, b"table_set\0".as_ptr() as *const _, ftype);
            extra_fns.insert("table_set".to_string(), (f, ftype));
        }

        // declare table_get
        {
            let mut args = [value_type, value_type];
            let ftype = LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as u32, 0);
            let f = LLVMAddFunction(module, b"table_get\0".as_ptr() as *const _, ftype);
            extra_fns.insert("table_get".to_string(), (f, ftype));
        }

        // declare num
        {
            let mut args = [f64_type];
            let ftype = LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as u32, 0);
            let f = LLVMAddFunction(module, b"num\0".as_ptr() as *const _, ftype);
            extra_fns.insert("num".to_string(), (f, ftype));
        }

        // declare nil
        {
            let mut args = [];
            let ftype = LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as u32, 0);
            let f = LLVMAddFunction(module, b"nil\0".as_ptr() as *const _, ftype);
            extra_fns.insert("nil".to_string(), (f, ftype));
        }

        // declare new_table
        {
            let mut args = [];
            let ftype = LLVMFunctionType(value_type, args.as_mut_ptr(), args.len() as u32, 0);
            let f = LLVMAddFunction(module, b"new_table\0".as_ptr() as *const _, ftype);
            extra_fns.insert("new_table".to_string(), (f, ftype));
        }

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

        compile_mainfn(&ir.fns[ir.main_fn], ir, &mut ctxt);

        LLVMDumpModule(ctxt.module);
    }
}

fn nil(ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let mut args = [];
        let (nil, nil_type) = ctxt.extra_fns["nil"].clone();

        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ nil_type,
            /*Fn: */ nil,
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        )
    }
}

fn compile_expr(e: &Expr, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        match e {
            Expr::Num(x) => {
                let mut args = [LLVMConstReal(ctxt.f64_type, *x)];
                let (num, num_type) = ctxt.extra_fns["num"].clone();

                LLVMBuildCall2(
                    /*builder: */ ctxt.builder,
                    /*type: */ num_type,
                    /*Fn: */ num,
                    /*Args: */ args.as_mut_ptr(),
                    /*Num Args: */ args.len() as u32,
                    /*Name: */ EMPTY,
                )
            },
            Expr::Nil => nil(ctxt),
            Expr::NewTable => {
                let mut args = [];
                let (new_table, new_table_type) = ctxt.extra_fns["new_table"].clone();

                LLVMBuildCall2(
                    /*builder: */ ctxt.builder,
                    /*type: */ new_table_type,
                    /*Fn: */ new_table,
                    /*Args: */ args.as_mut_ptr(),
                    /*Num Args: */ args.len() as u32,
                    /*Name: */ EMPTY,
                )
            }
            Expr::NativeFn(s) => {
                println!("ignoring Expr::NativeFn");

                nil(ctxt)
            },
            Expr::Index(t, i) => {
                let mut args = [ctxt.nodes[t], ctxt.nodes[i]];

                let (table_get, table_get_type) = ctxt.extra_fns["table_get"].clone();
                LLVMBuildCall2(
                    /*builder: */ ctxt.builder,
                    /*type: */ table_get_type,
                    /*Fn: */ table_get,
                    /*Args: */ args.as_mut_ptr(),
                    /*Num Args: */ args.len() as u32,
                    /*Name: */ EMPTY,
                )
            },
            Expr::FnCall(_, arg) => {
                println!("assuming FnCall is print");

                let mut args = [ctxt.nodes[arg]];

                let (print, print_type) = ctxt.extra_fns["print"].clone();
                LLVMBuildCall2(
                    /*builder: */ ctxt.builder,
                    /*type: */ print_type,
                    /*Fn: */ print,
                    /*Args: */ args.as_mut_ptr(),
                    /*Num Args: */ args.len() as u32,
                    /*Name: */ EMPTY,
                )
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
                    let mut args = [ctxt.nodes[t], ctxt.nodes[i], ctxt.nodes[v]];
                    let (table_set, table_set_type) = ctxt.extra_fns["table_set"].clone();
                    LLVMBuildCall2(
                        /*builder: */ ctxt.builder,
                        /*type: */ table_set_type,
                        /*Fn: */ table_set,
                        /*Args: */ args.as_mut_ptr(),
                        /*Num Args: */ args.len() as u32,
                        /*Name: */ EMPTY,
                    );
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
