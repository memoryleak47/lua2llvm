use crate::*;

use std::mem;

use llvm::core::*;
use llvm::execution_engine::*;
use llvm::target::*;

impl Ast {
    // prints the corresponding module to stdout
    pub fn compile(&self) {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
            let builder = LLVMCreateBuilderInContext(context);

            // get a type for main function
            let i32t: *mut llvm::LLVMType = LLVMInt32TypeInContext(context);
            let mut argts = [];
            let function_type = LLVMFunctionType(i32t, argts.as_mut_ptr(), 0, 0);

            // add it to our module
            let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);

            // Create a basic block in the function and set our builder to generate
            // code in it.
            let bb = LLVMAppendBasicBlockInContext(context, function, b"entry\0".as_ptr() as *const _);

            LLVMPositionBuilderAtEnd(builder, bb);

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
