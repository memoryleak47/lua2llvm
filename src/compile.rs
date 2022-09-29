use crate::*;

use llvm::core::*;

impl Ast {
    // prints the corresponding module to stdout
    pub fn compile(&self) {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
            let builder = LLVMCreateBuilderInContext(context);

            let i32t = LLVMInt32TypeInContext(context);
            let voidt = LLVMVoidType();

            // declare print
            let mut argts_print = [i32t];
            let print_type = LLVMFunctionType(voidt, argts_print.as_mut_ptr(), 1, 0);
            let print_function = LLVMAddFunction(module, b"extra_print\0".as_ptr() as *const _, print_type);

            // nop function
            let nop_type = LLVMFunctionType(voidt, [].as_mut_ptr(), 0, 0);
            let nop_function = LLVMAddFunction(module, b"nop\0".as_ptr() as *const _, nop_type);

            let bb = LLVMAppendBasicBlockInContext(context, nop_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(builder, bb);
            LLVMBuildRetVoid(builder);

            // create main
            let main_function_type = LLVMFunctionType(i32t, [].as_mut_ptr(), 0, 0);
            let main_function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, main_function_type);

            let bb = LLVMAppendBasicBlockInContext(context, main_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(builder, bb);

            for st in &self.statements {
                match st {
                    Statement::Print(_expr) => {
                        let mut args = [];
                        let t = LLVMFunctionType(voidt, [].as_mut_ptr(), 0, 0);
                        LLVMBuildCall2(
                            /*builder: */ builder,
                            /*type: */ t,
                            /*Fn: */ nop_function,
                            /*Args: */ args.as_mut_ptr(),
                            /*Num Args: */ 0,
                            /*Name: */ [0i8].as_ptr(),
                        );
                    },
                    _ => println!("not yet done! ignoring.."),
                }
            }

            let zero = LLVMConstInt(i32t, 0, 0);
            LLVMBuildRet(builder, zero);

            // done building
            LLVMDisposeBuilder(builder);

            // Dump the module as IR to stdout.
            LLVMDumpModule(module);

            LLVMContextDispose(context);
        }
    }
}
