use super::*;

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

pub fn mk_num(x: f64, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let val = LLVMConstReal(ctxt.f64_t(), x);
        let val = LLVMBuildBitCast(ctxt.builder, val, ctxt.i64_t(), EMPTY);

        let mut vals = [
            LLVMConstInt(ctxt.i32_t(), Tag::NUM as _, 0),
            LLVMGetUndef(ctxt.i32_t()),
            val
        ];
        LLVMConstStructInContext(ctxt.llctxt, vals.as_mut_ptr(), vals.len() as _, 0)
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
