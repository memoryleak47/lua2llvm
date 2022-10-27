use super::*;

pub fn compile_start_fn(main_fn: FnId, ctxt: &mut Ctxt) {
    unsafe {
        let start_fn = ctxt.start_fn;

        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, start_fn, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        let out_val = alloc(ctxt);
        let in_val = alloc_val(mk_nil(ctxt), ctxt);

        let mut args = [
            in_val,
            LLVMConstInt(ctxt.i32_t(), 0, 0),
            out_val,
        ];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2v_t(),
            /*Fn: */ ctxt.lit_fns[&main_fn],
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        );

        LLVMBuildRetVoid(ctxt.builder);
    }
}