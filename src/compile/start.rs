use super::*;

pub fn compile_start_fn(main_fn: FnId, ctxt: &mut Ctxt) {
    let start_function_type = ll::Type::Function(Box::new(ll::Type::Void), vec![]);
    let start_fn = ctxt.b.alloc_fn("main", start_function_type);

    ctxt.b.set_active_fn(start_fn);

    let bb = ctxt.b.alloc_block();
    ctxt.b.set_active_block(bb);
    ctxt.b.set_start_block(bb);

    let in_val = alloc_val(mk_nil(ctxt), ctxt);

    let f = ctxt.lit_fns[&main_fn];
    let args = vec![in_val];
    let ty = ctxt.v2void_t();
    ctxt.b.push_fn_call(f, args, ty);

    ctxt.b.push_return_void();
}
