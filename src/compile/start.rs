use super::*;

pub fn compile_start_fn(main_fn: FnId, ctxt: &mut Ctxt) {
    let start_fn = ctxt.start_fn;
    ctxt.current_ll_fn = start_fn;

    let bb = ctxt.alloc_block();
    ctxt.current_ll_bid = bb;

    let in_val = alloc_val(mk_nil(ctxt), ctxt);

    let f = ctxt.lit_fns[&main_fn];
    let f = ll::ValueId::Global(f);
    let args = vec![in_val];
    let ty = ctxt.v2void_t();
    ctxt.push_compute(ll::Expr::FnCall(f, args, ty));

    ctxt.push_st(ll::Statement::Return(None));
}
