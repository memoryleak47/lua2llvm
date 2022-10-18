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
        // declare the print function
        let mut argts_print = [ctxt.i32t];
        let print_type = LLVMFunctionType(ctxt.voidt, argts_print.as_mut_ptr(), 1, 0);
        let print = LLVMAddFunction(ctxt.module, b"print\0".as_ptr() as *const _, print_type);

        compile_mainfn(&ir.fns[ir.main_fn], print, ir, ctxt);
    }
}

fn compile_mainfn(f: &LitFunction, print: LLVMValueRef, ir: &IR, ctxt: &mut Ctxt) {
    unsafe {
        // create main
        let main_function_type = LLVMFunctionType(ctxt.i32t, [].as_mut_ptr(), 0, 0);
        let main_function = LLVMAddFunction(ctxt.module, b"main\0".as_ptr() as *const _, main_function_type);

        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.context, main_function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        let n12 = LLVMConstInt(ctxt.i32t, 12, 0);
        let mut args: Vec<LLVMValueRef> = vec![n12];
        let mut fn_ty_args: Vec<LLVMTypeRef> = args.iter().map(|_| ctxt.i32t).collect();
        let fn_ty = LLVMFunctionType(ctxt.voidt, fn_ty_args.as_mut_ptr(), fn_ty_args.len() as u32, 0);
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ fn_ty,
            /*Fn: */ print,
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        );

        let zero = LLVMConstInt(ctxt.i32t, 0, 0);
        LLVMBuildRet(ctxt.builder, zero);
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
