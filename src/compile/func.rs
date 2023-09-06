use super::*;
use std::collections::HashMap;

unsafe fn compile_block(block: &[Statement], bb_mapping: &HashMap<BlockId, LLVMBasicBlockRef>, ctxt: &mut Ctxt) {
    for st in block {
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
            Statement::If(cond, then_bid, else_bid) => {
                let cond = ctxt.nodes[cond];
                let cond = extract_bool(cond, ctxt);

                let thenbb = bb_mapping[then_bid];
                let elsebb = bb_mapping[else_bid];
                LLVMBuildCondBr(ctxt.builder, cond, thenbb, elsebb);

                return;
            },
            Statement::FnCall(f, arg) => {
                let f = ctxt.nodes[f];
                let arg = ctxt.nodes[arg];

                fn_call(f, arg, ctxt);
            }
            Statement::Command(cmd) => compile_command(cmd, ctxt),
            Statement::Return => {
                LLVMBuildRetVoid(ctxt.builder);
            },
        }
    }
}

unsafe fn compile_command(cmd: &Command, ctxt: &mut Ctxt) {
    match cmd {
        Command::Print(var) => {
            let t = alloc_val(ctxt.nodes[var], ctxt);
            call_extra_fn("print", &[t], ctxt);
        },
        Command::Throw(s) => {
            let s = format!("{}\0", s);
            let s = LLVMBuildGlobalString(ctxt.builder, s.as_ptr() as *const _, EMPTY);
            call_extra_fn("throw_", &[s], ctxt);
            LLVMBuildRetVoid(ctxt.builder); // required, even though this function will never return due to the above throw_.
        },
    }
}

unsafe fn fn_call(f_val: LLVMValueRef /* Value with FN tag */, arg: LLVMValueRef /* Value */, ctxt: &mut Ctxt) {
    // check tag
    let t = tag_err(f_val, Tag::FN, ctxt);
    err_chk(t, "trying to call non-function!", ctxt);

    // call fn
    let f /* i64 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 1, EMPTY);
    let f /* void (*fn)(Value) */ = LLVMBuildIntToPtr(ctxt.builder, f, ctxt.v2void_ptr_t(), EMPTY);

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


unsafe fn extract_bool(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* i1 */ {
    let val = LLVMBuildExtractValue(ctxt.builder, x, 1, EMPTY);
    let one = LLVMConstInt(ctxt.i64_t(), 1, 0);
    let ret = LLVMBuildICmp(ctxt.builder, LLVMIntPredicate::LLVMIntEQ, val, one, EMPTY);

    ret
}

pub unsafe fn compile_fn(val_f: LLVMValueRef, fn_id: FnId, ir: &IR, ctxt: &mut Ctxt) {
    ctxt.current_fid = fn_id;
    let f = &ir.fns[fn_id];

    let alloca_bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, b"entry\0".as_ptr() as *const _);

    let mut bb_mapping: HashMap<BlockId, LLVMBasicBlockRef> = HashMap::new();
    for (i, _) in f.blocks.iter().enumerate() {
        let new_blk = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, EMPTY);
        bb_mapping.insert(i, new_blk);
    }

    LLVMPositionBuilderAtEnd(ctxt.builder, alloca_bb);
    ctxt.alloca_br_instr = Some(LLVMBuildBr(ctxt.builder, bb_mapping[&f.start_block]));

    for (i, x) in f.blocks.iter().enumerate() {
        LLVMPositionBuilderAtEnd(ctxt.builder, bb_mapping[&i]);
        compile_block(&x, &bb_mapping, ctxt);
    }
}
