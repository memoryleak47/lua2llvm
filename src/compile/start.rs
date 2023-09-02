use super::*;

pub unsafe fn compile_start_fn(main_fn: FnId, ctxt: &mut Ctxt) {
    let start_fn = ctxt.start_fn;

    let alloca_bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, start_fn, b"entry\0".as_ptr() as *const _);
    let bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, start_fn, EMPTY);
    LLVMPositionBuilderAtEnd(ctxt.builder, alloca_bb);
    ctxt.alloca_br_instr = Some(LLVMBuildBr(ctxt.builder, bb));
    LLVMPositionBuilderAtEnd(ctxt.builder, bb);

    let in_val = alloc_val(mk_nil(ctxt), ctxt);

    let mut args = [in_val];
    LLVMBuildCall2(
        /*builder: */ ctxt.builder,
        /*type: */ ctxt.v2void_t(),
        /*Fn: */ ctxt.lit_fns[&main_fn],
        /*Args: */ args.as_mut_ptr(),
        /*Num Args: */ args.len() as u32,
        /*Name: */ EMPTY,
    );

    LLVMBuildRetVoid(ctxt.builder);
}
