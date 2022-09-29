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

            // declare print
            let mut argts_print = [i32t];
            let print_type = LLVMFunctionType(voidt, argts_print.as_mut_ptr(), 1, 0);
            let print_function = LLVMAddFunction(module, b"extra_print\0".as_ptr() as *const _, print_type);

            // nop function
            let mut argts_nop = [];
            let nop_type = LLVMFunctionType(voidt, argts_nop.as_mut_ptr(), 0, 0);
            let nop_function = LLVMAddFunction(module, b"nop\0".as_ptr() as *const _, nop_type);

            let bb = LLVMAppendBasicBlockInContext(context, nop_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(builder, bb);
            LLVMBuildRetVoid(builder);

            // create main
            let mut argts = [];
            let main_function_type = LLVMFunctionType(i32t, argts.as_mut_ptr(), 0, 0);
            let main_function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, main_function_type);

            let bb = LLVMAppendBasicBlockInContext(context, main_function, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(builder, bb);

            for st in &self.statements {
                match st {
                    Statement::Print(_expr) => {
                        let mut args = [];
                        LLVMBuildCall(
                            /*builder: */ builder,
                            // /*type: */ voidt,
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
