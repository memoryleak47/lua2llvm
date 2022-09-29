use crate::*;

use llvm::core::*;

impl Ast {
    // prints the corresponding module to stdout
    pub fn compile(&self) {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
            let builder: *mut llvm::LLVMBuilder = LLVMCreateBuilderInContext(context);

            let i32t: *mut llvm::LLVMType = LLVMInt32TypeInContext(context);
            let voidt = LLVMVoidType();

            // create main
            let mut argts = [];
            let main_function_type = LLVMFunctionType(i32t, argts.as_mut_ptr(), 0, 0);
            let main_function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, main_function_type);

            let bb = LLVMAppendBasicBlockInContext(context, main_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(builder, bb);

            // create print
            let mut argts_print = [i32t];
            let print_type = LLVMFunctionType(voidt, argts_print.as_mut_ptr(), 1, 0);
            let print_function = LLVMAddFunction(module, b"extra_print\0".as_ptr() as *const _, print_type);

            for st in &self.statements {
                match st {
                    Statement::Print(_expr) => {
                        let args_ref = &mut LLVMConstInt(i32t, 0, 0);
                        LLVMBuildCall2(
                            /*builder: */ builder,
                            /*type: */ voidt, // right? is that the return type?
                            /*Fn: */ print_function,
                            /*Args: */ args_ref as *mut _,
                            /*Num Args: */ 1,
                            /*Name: */ b"tmpresult\0".as_ptr() as *const i8,
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
