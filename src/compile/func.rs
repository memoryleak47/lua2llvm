use super::*;
use std::collections::{HashMap, HashSet};

unsafe fn compile_block(block: &[Statement], bb_mapping: &HashMap<BlockId, LLVMBasicBlockRef>, ctxt: &mut Ctxt) -> /*next blocks*/ Vec<BlockId> {
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

                return vec![*then_bid, *else_bid];
            },
            Statement::FnCall(f, arg) => {
                let f = ctxt.nodes[f];
                let arg = ctxt.nodes[arg];

                fn_call(f, arg, ctxt);
            }
            Statement::Print(var) => {
                let t = alloc_val(ctxt.nodes[var], ctxt);
                call_extra_fn("print", &[t], ctxt);
            },
            Statement::Throw(s) => {
                let s = format!("{}\0", s);
                let s = LLVMBuildGlobalString(ctxt.builder, s.as_ptr() as *const _, EMPTY);
                call_extra_fn("throw_", &[s], ctxt);
                LLVMBuildRetVoid(ctxt.builder); // required, even though this function will never return due to the above throw_.
            },
            Statement::Return => {
                LLVMBuildRetVoid(ctxt.builder);
            },
        }
    }

    return Vec::new();
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
    let f = &ir.fns[&fn_id];

    let alloca_bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, b"entry\0".as_ptr() as *const _);

    let mut bb_mapping: HashMap<BlockId, LLVMBasicBlockRef> = HashMap::new();
    for (bid, _) in f.blocks.iter() {
        let new_blk = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, EMPTY);
        bb_mapping.insert(*bid, new_blk);
    }

    LLVMPositionBuilderAtEnd(ctxt.builder, alloca_bb);
    ctxt.alloca_br_instr = Some(LLVMBuildBr(ctxt.builder, bb_mapping[&f.start_block]));

    let mut open_blocks = HashSet::new();
    open_blocks.insert(f.start_block);
    let mut done_blocks = HashSet::new();

    // TODO this "pick earliest" is a hack it shouldn't matter.
    // It currently matter sometimes because there are paths through the CFG
    // that don't initialize a node even though its used.
    // This isn't a "real" problem though, as these paths through the CFG
    // go through some "never-executed" blocks as detected by the Infer.
    while let Some(bid) = open_blocks.iter().min().copied() {
        open_blocks.remove(&bid);
        done_blocks.insert(bid);

        LLVMPositionBuilderAtEnd(ctxt.builder, bb_mapping[&bid]);
        let blk = &f.blocks[&bid];
        for new_bid in compile_block(blk, &bb_mapping, ctxt) {
            if !done_blocks.contains(&new_bid) {
                open_blocks.insert(new_bid);
            }
        }
    }
}
