use crate::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

pub fn compile(ast: &Ast) {
    Compile::new()
        .compile_ast(ast)
}

struct Compile {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    i32t: LLVMTypeRef,
    voidt: LLVMTypeRef,
    vars: HashMap<String, LLVMValueRef>,
    fns: HashMap<String, LLVMValueRef>,
    bb: LLVMBasicBlockRef,
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
            let fns = HashMap::new();
            let bb = 0 as *mut _;

            Compile {
                context,
                module,
                builder,
                i32t,
                voidt,
                vars,
                fns,
                bb,
            }
        }
    }

    fn compile_ast(&mut self, ast: &Ast) {
        unsafe {
            let mut argts_print = [self.i32t];
            let print_type = LLVMFunctionType(self.voidt, argts_print.as_mut_ptr(), 1, 0);
            let print = LLVMAddFunction(self.module, b"extra_print\0".as_ptr() as *const _, print_type);
            self.fns.insert("print".to_string(), print); 

            // create main
            let main_function_type = LLVMFunctionType(self.i32t, [].as_mut_ptr(), 0, 0);
            let main_function = LLVMAddFunction(self.module, b"main\0".as_ptr() as *const _, main_function_type);

            self.bb = LLVMAppendBasicBlockInContext(self.context, main_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(self.builder, self.bb);

            self.compile(&ast.statements);

            let zero = LLVMConstInt(self.i32t, 0, 0);
            LLVMBuildRet(self.builder, zero);
        }
    }
    fn compile(&mut self, stmts: &[Statement]) {
        unsafe {
            for st in stmts {
                match st {
                    Statement::FunctionCall { fn_name, args } => {
                        let mut args: Vec<LLVMValueRef> = args.iter().map(|expr| self.compile_expr(expr)).collect();
                        let mut fn_ty_args: Vec<LLVMTypeRef> = args.iter().map(|_| self.i32t).collect();
                        let fn_ty = LLVMFunctionType(self.voidt, fn_ty_args.as_mut_ptr(), fn_ty_args.len() as u32, 0);
                        LLVMBuildCall2(
                            /*builder: */ self.builder,
                            /*type: */ fn_ty,
                            /*Fn: */ self.fns[&fn_name.to_string()],
                            /*Args: */ args.as_mut_ptr(),
                            /*Num Args: */ args.len() as u32,
                            /*Name: */ EMPTY,
                        );
                    },
                    Statement::Assign { var, expr } => {
                        let val = self.compile_expr(expr);
                        self.vars.insert(var.clone(), val);
                    },
                    Statement::FunctionDef { fn_name, args, body } => {
                        if args.len() > 0 {
                            println!("ignoring multi-arg fn...");
                            continue;
                        }
                        let mut fn_ty_args: Vec<LLVMTypeRef> = args.iter().map(|_| self.i32t).collect();
                        let fn_ty = LLVMFunctionType(self.voidt, fn_ty_args.as_mut_ptr(), fn_ty_args.len() as u32, 0);
                        let fun = LLVMAddFunction(self.module, EMPTY, fn_ty);

                        let old_bb = self.bb;
                        self.bb = LLVMAppendBasicBlockInContext(self.context, fun, EMPTY);
                        LLVMPositionBuilderAtEnd(self.builder, self.bb);
                        self.compile(&body);
                        LLVMBuildRetVoid(self.builder);

                        self.bb = old_bb;
                        LLVMPositionBuilderAtEnd(self.builder, self.bb);

                        self.fns.insert(fn_name.clone(), fun);
                    },
                }
            }
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
