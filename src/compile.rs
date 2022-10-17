use crate::ir::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

struct Ctxt {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    i32t: LLVMTypeRef,
    voidt: LLVMTypeRef,
    bb: LLVMBasicBlockRef,

    nodes: HashMap<Node, LLVMValueRef>,
}

pub fn compile(ir: &IR) {
    let mut ctxt = new_ctxt();
    compile_ir(ir, &mut ctxt);

    unsafe {
        LLVMDumpModule(ctxt.module);
    }
}

fn new_ctxt() -> Ctxt {
    unsafe {
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, context);
        let builder = LLVMCreateBuilderInContext(context);

        let i32t = LLVMInt32TypeInContext(context);
        let voidt = LLVMVoidType();
        let bb = 0 as *mut _;

        let nodes = HashMap::new();

        Ctxt {
            context,
            module,
            builder,
            i32t,
            voidt,
            bb,
            nodes,
        }
    }
}

fn compile_ir(ir: &IR, ctxt: &mut Ctxt) {
    unsafe {
/*
        let mut argts_print = [ctxt.i32t];
        let print_type = LLVMFunctionType(ctxt.voidt, argts_print.as_mut_ptr(), 1, 0);
        let print = LLVMAddFunction(ctxt.module, b"extra_print\0".as_ptr() as *const _, print_type);
        // ctxt.fns.insert("print".to_string(), print); 
*/

        // create main
        let main_function_type = LLVMFunctionType(ctxt.i32t, [].as_mut_ptr(), 0, 0);
        let main_function = LLVMAddFunction(ctxt.module, b"main\0".as_ptr() as *const _, main_function_type);

        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.context, main_function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        for f in &ir.fns {
            compile_fn(f, ctxt);
        }

        let zero = LLVMConstInt(ctxt.i32t, 0, 0);
        LLVMBuildRet(ctxt.builder, zero);
    }
}

fn compile_fn(func: &LitFunction, ctxt: &mut Ctxt) {
    unsafe {
        for st in &func.body {
            match st {
               _ => {/* TODO */},
            }
        }
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
