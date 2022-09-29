use crate::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

pub fn compile(ast: &Ast) {
    Compile::new()
        .compile(ast)
}

struct Compile {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    i32t: LLVMTypeRef,
    voidt: LLVMTypeRef,
    vars: HashMap<String, LLVMValueRef>,
}

impl Compile {
    fn new() -> Compile {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
            let builder = LLVMCreateBuilderInContext(context);

            let i32t = LLVMInt32TypeInContext(context);
            let voidt = LLVMVoidType();
            let vars = HashMap::new();

            Compile {
                context,
                module,
                builder,
                i32t,
                voidt,
                vars,
            }
        }
    }

    fn compile(&mut self, ast: &Ast) {
        unsafe {
            let mut argts_print = [self.i32t];
            let print_type = LLVMFunctionType(self.voidt, argts_print.as_mut_ptr(), 1, 0);
            let print_function = LLVMAddFunction(self.module, b"extra_print\0".as_ptr() as *const _, print_type);

            // create main
            let main_function_type = LLVMFunctionType(self.i32t, [].as_mut_ptr(), 0, 0);
            let main_function = LLVMAddFunction(self.module, b"main\0".as_ptr() as *const _, main_function_type);

            let bb = LLVMAppendBasicBlockInContext(self.context, main_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(self.builder, bb);

            for st in &ast.statements {
                match st {
                    Statement::Print(expr) => {
                        let mut args = [self.compile_expr(expr)];
                        LLVMBuildCall2(
                            /*builder: */ self.builder,
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

            let zero = LLVMConstInt(self.i32t, 0, 0);
            LLVMBuildRet(self.builder, zero);
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> LLVMValueRef {
        unsafe {
            match expr {
                Expr::Var(var) => {
                    self.vars[var]
                },
                Expr::LiteralNum(x) => {
                    LLVMConstInt(self.i32t, *x as u64, 0)
                },
                Expr::Plus(l, r) => {
                    let l = self.compile_expr(l);
                    let r = self.compile_expr(r);
                    LLVMBuildAdd(self.builder, l, r, EMPTY)
                },
            }
        }
    }
}

impl Drop for Compile {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDumpModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
