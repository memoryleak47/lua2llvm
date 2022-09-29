use crate::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

// prints the corresponding module to stdout
pub fn compile(ast: &Ast) {
    unsafe {
        // Set up a context, module and builder in that context.
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
        let builder = LLVMCreateBuilderInContext(context);

        let i32t = LLVMInt32TypeInContext(context);
        let voidt = LLVMVoidType();
        let zero = LLVMConstInt(i32t, 0, 0);

        // declare print
        let mut argts_print = [i32t];
        let print_type = LLVMFunctionType(voidt, argts_print.as_mut_ptr(), 1, 0);
        let print_function = LLVMAddFunction(module, b"extra_print\0".as_ptr() as *const _, print_type);

        // create main
        let main_function_type = LLVMFunctionType(i32t, [].as_mut_ptr(), 0, 0);
        let main_function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, main_function_type);

        let bb = LLVMAppendBasicBlockInContext(context, main_function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut vars: HashMap<String, LLVMValueRef> = HashMap::new();

        for st in &ast.statements {
            match st {
                Statement::Print(expr) => {
                    let mut args = [compile_expr(context, builder, expr, &mut vars)];
                    LLVMBuildCall2(
                        /*builder: */ builder,
                        /*type: */ print_type,
                        /*Fn: */ print_function,
                        /*Args: */ args.as_mut_ptr(),
                        /*Num Args: */ args.len() as u32,
                        /*Name: */ EMPTY,
                    );
                },
                _ => println!("not yet done! ignoring.."),
            }
        }

        LLVMBuildRet(builder, zero);

        // done building
        LLVMDisposeBuilder(builder);

        // Dump the module as IR to stdout.
        LLVMDumpModule(module);

        LLVMContextDispose(context);
    }
}

fn compile_expr(context: LLVMContextRef, builder: LLVMBuilderRef, expr: &Expr, vars: &mut HashMap<String, LLVMValueRef>) -> LLVMValueRef {
    unsafe {
        match expr {
            Expr::Var(var) => {
                vars[var]
            },
            Expr::LiteralNum(x) => {
                let i32t = LLVMInt32TypeInContext(context);
                LLVMConstInt(i32t, *x as u64, 0)
            },
            Expr::Plus(l, r) => {
                let l = compile_expr(context, builder, l, vars);
                let r = compile_expr(context, builder, r, vars);
                LLVMBuildAdd(builder, l, r, EMPTY)
            },
        }
    }
}
