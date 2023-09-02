use super::*;

// error handling

pub unsafe fn err_chk(err_cond: LLVMValueRef, error_msg: &str, ctxt: &mut Ctxt) {
    let errblock = LLVMAppendBasicBlockInContext(ctxt.llctxt, ctxt.lit_fns[&ctxt.current_fid], EMPTY);
    let goodblock = LLVMAppendBasicBlockInContext(ctxt.llctxt, ctxt.lit_fns[&ctxt.current_fid], EMPTY);

    LLVMBuildCondBr(ctxt.builder, err_cond, errblock, goodblock);

    LLVMPositionBuilderAtEnd(ctxt.builder, errblock);

    let s = format!("{}\0", error_msg);
    let s = LLVMBuildGlobalString(ctxt.builder, s.as_ptr() as *const _, EMPTY);
    let s = LLVMBuildBitCast(ctxt.builder, s, ctxt.str_t(), EMPTY);
    call_extra_fn("puts", &[s], ctxt);

    let one = LLVMConstInt(ctxt.i32_t(), 1, 0);
    call_extra_fn("exit", &[one], ctxt);

    LLVMBuildUnreachable(ctxt.builder);

    LLVMPositionBuilderAtEnd(ctxt.builder, goodblock);
}

// returns `true` if err was found.
pub unsafe fn tag_err(v: LLVMValueRef, tag: Tag, ctxt: &mut Ctxt) -> LLVMValueRef /* i8 */ {
    let tag = LLVMConstInt(ctxt.i64_t(), tag as _, 0);

    let vtag = LLVMBuildExtractValue(ctxt.builder, v, 0, EMPTY);
    LLVMBuildICmp(ctxt.builder, LLVMIntPredicate::LLVMIntNE, vtag, tag, EMPTY)
}

pub unsafe fn mk_nil(ctxt: &mut Ctxt) -> LLVMValueRef {
    let mut vals = [
        LLVMConstInt(ctxt.i64_t(), Tag::NIL as _, 0),
        LLVMGetUndef(ctxt.i64_t())
    ];
    LLVMConstStructInContext(ctxt.llctxt, vals.as_mut_ptr(), vals.len() as _, 0)
}

pub unsafe fn mk_num(x: LLVMValueRef /* f64 */, ctxt: &mut Ctxt) -> LLVMValueRef {
    let x = LLVMBuildBitCast(ctxt.builder, x, ctxt.i64_t(), EMPTY);
    let tag = LLVMConstInt(ctxt.i64_t(), Tag::NUM as _, 0);

    let a = LLVMGetPoison(ctxt.value_t());
    let a = LLVMBuildInsertValue(ctxt.builder, a, tag, 0, EMPTY);
    let a = LLVMBuildInsertValue(ctxt.builder, a, x, 1, EMPTY);

    a
}

pub unsafe fn mk_str(s: LLVMValueRef /* i8* */, ctxt: &mut Ctxt) -> LLVMValueRef {
    let tag = LLVMConstInt(ctxt.i64_t(), Tag::STR as _, 0);

    let s = LLVMBuildPtrToInt(ctxt.builder, s, ctxt.i64_t(), EMPTY);

    let a = LLVMGetPoison(ctxt.value_t());
    let a = LLVMBuildInsertValue(ctxt.builder, a, tag, 0, EMPTY);
    let a = LLVMBuildInsertValue(ctxt.builder, a, s, 1, EMPTY);

    a
}

pub unsafe fn mk_bool(x: LLVMValueRef /* i1 */, ctxt: &mut Ctxt) -> LLVMValueRef {
    let x = LLVMBuildZExt(ctxt.builder, x, ctxt.i64_t(), EMPTY);
    let tag = LLVMConstInt(ctxt.i64_t(), Tag::BOOL as _, 0);

    let a = LLVMGetPoison(ctxt.value_t());
    let a = LLVMBuildInsertValue(ctxt.builder, a, tag, 0, EMPTY);
    let a = LLVMBuildInsertValue(ctxt.builder, a, x, 1, EMPTY);

    a
}

pub unsafe fn extract_num(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* f64 */ {
    let x = LLVMBuildExtractValue(ctxt.builder, x, 1, EMPTY);
    let x = LLVMBuildBitCast(ctxt.builder, x, ctxt.f64_t(), EMPTY);

    x
}

// f should be generated using LLVMAddFunction of type v2void_t.
pub unsafe fn mk_fn(f: LLVMValueRef, ctxt: &mut Ctxt) -> LLVMValueRef {
    let val = LLVMBuildPtrToInt(ctxt.builder, f, ctxt.i64_t(), EMPTY);

    let tag = LLVMConstInt(ctxt.i64_t(), Tag::FN as _, 0);

    let a = LLVMGetPoison(ctxt.value_t());
    let a = LLVMBuildInsertValue(ctxt.builder, a, tag, 0, EMPTY);
    let a = LLVMBuildInsertValue(ctxt.builder, a, val, 1, EMPTY);

    a
}
