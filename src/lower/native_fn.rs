use crate::lower::*;

type NativeImpls = HashMap<&'static str, FnId>;
static NATIVE_FNS: &'static [(&'static str, fn(&mut Ctxt, &NativeImpls))] = &[("print", print_native_fn), ("next", next_native_fn), ("type", type_native_fn), ("pairs", pairs_native_fn)];

// ctxt is currently implementing main at this point.
pub(in crate::lower) fn add_native_fns(ctxt: &mut Ctxt) {
    let mut native_impls: NativeImpls = HashMap::new();

    for (fn_ident, generator) in NATIVE_FNS.iter() {
        let (fn_id, ()) = add_fn(false, |ctxt| generator(ctxt, &native_impls), ctxt);
        native_impls.insert(fn_ident, fn_id);

        let fun = mk_table(ctxt);

        let upvalues = mk_table(ctxt);
        ctxt.push_store(fun, ctxt.upvalues_str(), upvalues);

        let call = ctxt.push_compute(ir::Expr::LitFunction(fn_id));
        ctxt.push_store(fun, ctxt.call_str(), call);

        // this table is required, as it's still a variable!
        let t = mk_table_with(fun, ctxt);
        ctxt.fcx_mut().locals.last_mut().unwrap().insert(String::from(*fn_ident), t);
    }
}

fn print_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    // TODO consider iterating over the table to print everything.
    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args = ctxt.push_compute(ir::Expr::Index(arg, ctxt.args_str()));
    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one()));
    let (is_fn, call_node) = mk_fn_check(arg1, ctxt);

    let then_bid = ctxt.alloc_block();
    let else_bid = ctxt.alloc_block();
    let post_bid = ctxt.alloc_block();

    ctxt.push_if(is_fn, then_bid, else_bid);

    ctxt.set_active_block(then_bid);
    ctxt.push_compute(ir::Expr::Intrinsic(ir::Intrinsic::Print(call_node)));
    ctxt.push_goto(post_bid);

    ctxt.set_active_block(else_bid);
    ctxt.push_compute(ir::Expr::Intrinsic(ir::Intrinsic::Print(arg1)));
    ctxt.push_goto(post_bid);

    ctxt.set_active_block(post_bid);
    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.count_str(), ctxt.zero());
    ctxt.push_store(arg, ctxt.retval_str(), ret);
}

fn type_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args = ctxt.push_compute(ir::Expr::Index(arg, ctxt.args_str()));
    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one()));
    let val = ctxt.push_compute(ir::Expr::Intrinsic(ir::Intrinsic::Type(arg1)));
    let (is_fn, _) = mk_fn_check(arg1, ctxt);

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.count_str(), ctxt.one());

    let then_bid = ctxt.alloc_block();
    let else_bid = ctxt.alloc_block();
    let post_bid = ctxt.alloc_block();

    ctxt.push_if(is_fn, then_bid, else_bid);

    ctxt.set_active_block(then_bid);
    ctxt.push_store(ret, ctxt.one(), ctxt.function_str());
    ctxt.push_goto(post_bid);

    ctxt.set_active_block(else_bid);
    ctxt.push_store(ret, ctxt.one(), val);
    ctxt.push_goto(post_bid);

    ctxt.set_active_block(post_bid);
    ctxt.push_store(arg, ctxt.retval_str(), ret);
}

fn next_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args = ctxt.push_compute(ir::Expr::Index(arg, ctxt.args_str()));
    let two = mk_num(2, ctxt);

    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one()));
    mk_assert(mk_proper_table_check(arg1, ctxt), "Argument to next is not a table!", ctxt);
    let arg2 = ctxt.push_compute(ir::Expr::Index(args, two));
    let new_index = ctxt.push_compute(ir::Expr::Intrinsic(ir::Intrinsic::Next(arg1, arg2)));
    let new_val = ctxt.push_compute(ir::Expr::Index(arg1, new_index));

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.count_str(), two);
    ctxt.push_store(ret, ctxt.one(), new_index);
    ctxt.push_store(ret, two, new_val);
    
    ctxt.push_store(arg, ctxt.retval_str(), ret);
}

fn pairs_native_fn(ctxt: &mut Ctxt, native_impls: &NativeImpls) {
    let two = mk_num(2, ctxt);
    let three = mk_num(3, ctxt);

    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args = ctxt.push_compute(ir::Expr::Index(arg, ctxt.args_str()));
    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one()));

    let next_table = mk_table(ctxt);
    let tmp = mk_table(ctxt);
    ctxt.push_store(next_table, ctxt.upvalues_str(), tmp);

    let next_fn = ctxt.push_compute(ir::Expr::LitFunction(native_impls["next"]));
    ctxt.push_store(next_table, ctxt.call_str(), next_fn);

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.count_str(), three);

    ctxt.push_store(ret, ctxt.one(), next_table);
    ctxt.push_store(ret, two, arg1);
    let nil_node = ctxt.push_compute(ir::Expr::Nil);
    ctxt.push_store(ret, three, nil_node);
    
    ctxt.push_store(arg, ctxt.retval_str(), ret);
}
