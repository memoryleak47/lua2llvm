use super::*;

enum Terminated { Yes, No }

fn compile_body(body: &[Statement], ctxt: &mut Ctxt) -> Terminated {
    unsafe {
        for st in body {
            match st {
                Statement::Compute(n, e) => {
                    let vref = compile_expr(e, ctxt);
                    ctxt.nodes.insert(*n, vref);
                },
                Statement::Store(t, i, v) => {
                    let t = alloc_val(ctxt.nodes[t], ctxt);
                    let i = alloc_val(ctxt.nodes[i], ctxt);
                    let v = alloc_val(ctxt.nodes[v], ctxt);
                    call_extra_fn("table_set", &[t, i, v], ctxt);
                },
                Statement::Return => {
                    LLVMBuildRetVoid(ctxt.builder);

                    return Terminated::Yes;
                },
                Statement::If(cond, thenbody, elsebody) => {
                    let f = ctxt.lit_fns[&ctxt.current_fid];

                    let cond = ctxt.nodes[cond];
                    let cond = truthy(cond, ctxt);

                    let thenbb = LLVMAppendBasicBlockInContext(ctxt.llctxt, f, EMPTY);
                    let elsebb = LLVMAppendBasicBlockInContext(ctxt.llctxt, f, EMPTY);
                    let postbb = LLVMAppendBasicBlockInContext(ctxt.llctxt, f, EMPTY);
                    LLVMBuildCondBr(ctxt.builder, cond, thenbb, elsebb);

                    LLVMPositionBuilderAtEnd(ctxt.builder, thenbb);
                    if let Terminated::No = compile_body(thenbody, ctxt) {
                        LLVMBuildBr(ctxt.builder, postbb);
                    }

                    LLVMPositionBuilderAtEnd(ctxt.builder, elsebb);
                    if let Terminated::No = compile_body(elsebody, ctxt) {
                        LLVMBuildBr(ctxt.builder, postbb);
                    }

                    LLVMPositionBuilderAtEnd(ctxt.builder, postbb);
                },
                Statement::Loop(body) => {
                    let f = ctxt.lit_fns[&ctxt.current_fid];

                    let loopbb = LLVMAppendBasicBlockInContext(ctxt.llctxt, f, EMPTY);
                    let postbb = LLVMAppendBasicBlockInContext(ctxt.llctxt, f, EMPTY);

                    LLVMBuildBr(ctxt.builder, loopbb);

                    { // loop block:
                        let previous_break_bb = ctxt.break_bb.take();
                        ctxt.break_bb = Some(postbb);
                        LLVMPositionBuilderAtEnd(ctxt.builder, loopbb);

                        if let Terminated::No = compile_body(body, ctxt) {
                            LLVMBuildBr(ctxt.builder, loopbb);
                        }

                        ctxt.break_bb = previous_break_bb;
                    }

                    LLVMPositionBuilderAtEnd(ctxt.builder, postbb);
                },
                Statement::FnCall(f, arg) => {
                    let f = ctxt.nodes[f];
                    let arg = ctxt.nodes[arg];

                    fn_call(f, arg, ctxt);
                }
                Statement::Break => {
                    let bb = ctxt.break_bb.unwrap();
                    LLVMBuildBr(ctxt.builder, bb);

                    return Terminated::Yes;
                },
            }
        }

        Terminated::No
    }
}

fn fn_call(f_val: LLVMValueRef /* Value with FN tag */, arg: LLVMValueRef /* Value */, ctxt: &mut Ctxt) {
    unsafe {
        // check tag
        let t = tag_err(f_val, Tag::FN, ctxt);
        err_chk(t, "trying to call non-function!", ctxt);

        // call fn
        let f /* i64 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 1, EMPTY);
        let f /* Value (*fn)(Value) */ = LLVMBuildIntToPtr(ctxt.builder, f, ctxt.v2void_ptr_t(), EMPTY);

        let mut fargs = [alloc_val(arg, ctxt)];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2void_t(),
            /*Fn: */ f,
            /*Args: */ fargs.as_mut_ptr(),
            /*Num Args: */ fargs.len() as u32,
            /*Name: */ EMPTY,
        );
    }
}


fn truthy(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* i1 */ {
    unsafe {
        let tag = LLVMBuildExtractValue(ctxt.builder, x, 0, EMPTY);
        let val = LLVMBuildExtractValue(ctxt.builder, x, 1, EMPTY);

        let niltag = LLVMConstInt(ctxt.i64_t(), Tag::NIL as _, 0);
        let tag_ne_nil = LLVMBuildICmp(ctxt.builder, LLVMIntPredicate::LLVMIntNE, tag, niltag, EMPTY);

        let booltag = LLVMConstInt(ctxt.i64_t(), Tag::BOOL as _, 0);
        let tag_ne_bool = LLVMBuildICmp(ctxt.builder, LLVMIntPredicate::LLVMIntNE, tag, booltag, EMPTY);

        let one = LLVMConstInt(ctxt.i64_t(), 1, 0);
        let is_true = LLVMBuildICmp(ctxt.builder, LLVMIntPredicate::LLVMIntEQ, one, val, EMPTY);

        // tag_ne_nil && (tag_ne_bool || is_true)
        let ret = LLVMBuildOr(ctxt.builder, tag_ne_bool, is_true, EMPTY);
        let ret = LLVMBuildAnd(ctxt.builder, tag_ne_nil, ret, EMPTY);

        ret
    }
}

pub fn compile_fn(val_f: LLVMValueRef, fn_id: FnId, ir: &IR, ctxt: &mut Ctxt) {
    unsafe {
        ctxt.current_fid = fn_id;
        let alloca_bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, b"entry\0".as_ptr() as *const _);
        let bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, EMPTY);

        LLVMPositionBuilderAtEnd(ctxt.builder, alloca_bb);
        ctxt.alloca_br_instr = Some(LLVMBuildBr(ctxt.builder, bb));

        LLVMPositionBuilderAtEnd(ctxt.builder, bb);

        let lit_f = &ir.fns[fn_id];
        compile_body(&lit_f.body, ctxt);
    }
}
