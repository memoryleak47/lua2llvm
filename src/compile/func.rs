use super::*;

fn compile_block(fid: FnId, bid: BlockId, ctxt: &mut Ctxt) {
    let f = ctxt.ir.fns[&fid].clone();
    let blk = &f.blocks[&bid];

    ctxt.b.set_active_block(ctxt.blocks[&bid]);

    for st in blk {
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

                let thenbb = ctxt.blocks[then_bid];
                let elsebb = ctxt.blocks[else_bid];
                ctxt.b.push_st(ll::Statement::CondBr(cond, thenbb, elsebb));
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
                let v = ctxt.b.alloc_string(s);
                call_extra_fn("throw_", &[v], ctxt);
                ctxt.b.push_st(ll::Statement::Return(None));
            },
            Statement::Return => {
                ctxt.b.push_st(ll::Statement::Return(None));
            },
        }
    }
}

fn fn_call(f_val: ll::ValueId /* Value with FN tag */, arg: ll::ValueId, ctxt: &mut Ctxt) {
    // check tag
    let t = tag_err(f_val, Tag::FN, ctxt);
    err_chk(t, "trying to call non-function!", ctxt);

    // call fn
    let f /* i64 */ = ctxt.b.push_compute(ll::Expr::ExtractValue(f_val, 1));
    let v2void_ptr_t = ctxt.v2void_ptr_t();
    let f /* void (*fn)(Value) */ = ctxt.b.push_compute(ll::Expr::IntToPtr(f, v2void_ptr_t));
    let args = vec![alloc_val(arg, ctxt)];
    let ty = ctxt.v2void_t();
    ctxt.b.push_compute(ll::Expr::FnCall(f, args, ty));
}


fn extract_bool(x: ll::ValueId /* Value */, ctxt: &mut Ctxt) -> ll::ValueId /* i1 */ {
    let val = ctxt.b.push_compute(ll::Expr::ExtractValue(x, 1));
    let i64_t = ctxt.i64_t();
    let one = ctxt.b.push_compute(ll::Expr::ConstInt(i64_t, 1));
    let ret = ctxt.b.push_compute(ll::Expr::NumOp(ll::NumOpKind::IsEqual, ll::NumKind::Int, val, one));

    ret
}

pub fn compile_fn(fid: FnId, ctxt: &mut Ctxt) {
    ctxt.blocks = Default::default();
    ctxt.nodes = Default::default();

    ctxt.b.set_active_fn(ctxt.lit_fns[&fid]);

    let f = ctxt.ir.fns[&fid].clone();

    let mut bids: Vec<BlockId> = f.blocks.keys().cloned().collect();
    bids.sort();

    for &bid in &bids {
        let new_blk = ctxt.b.alloc_block();
        ctxt.blocks.insert(bid, new_blk);
    }

    ctxt.b.set_start_block(ctxt.blocks[&f.start_block]);

    for &bid in &bids {
        compile_block(fid, bid, ctxt);
    }
}
