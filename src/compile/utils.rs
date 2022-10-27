use super::*;

// error handling

pub fn err_chk(err_cond: LLVMValueRef, error_msg: &str, ctxt: &mut Ctxt) {
    unsafe {
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

        ctxt.bb = goodblock;
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);
    }
}

// returns `true` if err was found.
pub fn tag_err(v: LLVMValueRef, tag: Tag, ctxt: &mut Ctxt) -> LLVMValueRef /* i8 */ {
    unsafe {
        let tag = LLVMConstInt(ctxt.i32_t(), tag as _, 0);

        let vtag = LLVMBuildExtractValue(ctxt.builder, v, 0, EMPTY);
        LLVMBuildICmp(ctxt.builder, LLVMIntPredicate::LLVMIntNE, vtag, tag, EMPTY)
    }
}

pub fn mk_nil(ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let mut vals = [
            LLVMConstInt(ctxt.i32_t(), Tag::NIL as _, 0),
            LLVMGetUndef(ctxt.i32_t()),
            LLVMGetUndef(ctxt.i64_t())
        ];
        LLVMConstStructInContext(ctxt.llctxt, vals.as_mut_ptr(), vals.len() as _, 0)
    }
}

pub fn mk_num(x: LLVMValueRef /* f64 */, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let x = LLVMBuildBitCast(ctxt.builder, x, ctxt.i64_t(), EMPTY);
        let tag = LLVMConstInt(ctxt.i32_t(), Tag::NUM as _, 0);

        let a = LLVMGetPoison(ctxt.value_t());
        let a = LLVMBuildInsertValue(ctxt.builder, a, tag, 0, EMPTY);
        let a = LLVMBuildInsertValue(ctxt.builder, a, x, 2, EMPTY);

        a
    }
}

pub fn extract_num(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* f64 */ {
    unsafe {
        let x = LLVMBuildExtractValue(ctxt.builder, x, 2, EMPTY);
        let x = LLVMBuildBitCast(ctxt.builder, x, ctxt.f64_t(), EMPTY);

        x
    }
}

// f should be generated using LLVMAddFunction of type v2v_t.
// uvstack_index needs to be 0 for 0-closure functions.
pub fn mk_fn(f: LLVMValueRef, uvstack_index: LLVMValueRef, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let val = LLVMBuildPtrToInt(ctxt.builder, f, ctxt.i64_t(), EMPTY);

        let tag = LLVMConstInt(ctxt.i32_t(), Tag::FN as _, 0);

        let a = LLVMGetPoison(ctxt.value_t());
        let a = LLVMBuildInsertValue(ctxt.builder, a, tag, 0, EMPTY);
        let a = LLVMBuildInsertValue(ctxt.builder, a, uvstack_index, 1, EMPTY);
        let a = LLVMBuildInsertValue(ctxt.builder, a, val, 2, EMPTY);

        a
    }
}

pub fn mk_natfn(i: NativeFnId, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let s = NATIVE_FNS[i];
        let f = ctxt.extra_fns[s].f;
        let zero = LLVMConstInt(ctxt.i32_t(), 0, 0);

        mk_fn(f, zero, ctxt)
    }
}
