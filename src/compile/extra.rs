use super::*;

pub fn declare_extra_fn(fname: &str, ret: LLVMTypeRef, args: &[LLVMTypeRef], ctxt: &mut Ctxt) {
    unsafe {
        let mut args: Vec<LLVMTypeRef> = args.iter().cloned().collect();
        let ftype = LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0);
        let name = format!("{}\0", fname);
        let f = LLVMAddFunction(ctxt.module, name.as_bytes().as_ptr() as *const _, ftype);
        let extra_fn = ExtraFn { f, ftype };
        ctxt.extra_fns.insert(fname.to_string(), extra_fn);
    }
}

pub fn call_extra_fn(fname: &str, args: &[LLVMValueRef], ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let extra_fn: &ExtraFn = ctxt.extra_fns.get(fname).unwrap_or_else(|| panic!("unknown extra fn \"{}\"", fname));
        let mut args: Vec<LLVMValueRef> = args.iter().cloned().collect();

        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ extra_fn.ftype,
            /*Fn: */ extra_fn.f,
            /*Args: */ args.as_mut_ptr(),
            /*Num Args: */ args.len() as u32,
            /*Name: */ EMPTY,
        )
    }
}


