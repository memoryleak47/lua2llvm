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

            // create main
            let main_function_type = LLVMFunctionType(i32t, [].as_mut_ptr(), 0, 0);
            let main_function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, main_function_type);

            let bb = LLVMAppendBasicBlockInContext(context, main_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(builder, bb);

            let zero = LLVMConstInt(i32t, 0, 0);
            for st in &self.statements {
                match st {
                    Statement::Print(_expr) => {
                        let mut args = [zero];
                        LLVMBuildCall2(
                            /*builder: */ builder,
                            /*type: */ print_type,
                            /*Fn: */ print_function,
                            /*Args: */ args.as_mut_ptr(),
                            /*Num Args: */ args.len() as u32,
                            /*Name: */ b"\0".as_ptr() as *const _,
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
}
