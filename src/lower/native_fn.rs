use crate::lower::*;

type NativeImpls = HashMap<&'static str, FnId>;
static NATIVE_FNS: &'static [(&'static str, fn(&mut Ctxt, &NativeImpls))] = &[("print", print_native_fn), ("next", next_native_fn), ("type", type_native_fn), ("pairs", pairs_native_fn)];

// ctxt is currently implementing main at this point.
pub(in crate::lower) fn add_native_fns(ctxt: &mut Ctxt) {
    let mut native_impls: NativeImpls = HashMap::new();

    let call_str = ctxt.push_compute(ir::Expr::Str(String::from("call")));
    let upvalues_str = ctxt.push_compute(ir::Expr::Str(String::from("upvalues")));

    for (fn_ident, generator) in NATIVE_FNS.iter() {
        let (fn_id, ()) = add_fn(|ctxt| generator(ctxt, &native_impls), ctxt);
        native_impls.insert(fn_ident, fn_id);

        let fun = mk_table(ctxt);

        let upvalues = mk_table(ctxt);
        ctxt.push_store(fun, upvalues_str, upvalues);

        let call = ctxt.push_compute(ir::Expr::LitFunction(fn_id));
        ctxt.push_store(fun, call_str, call);

        // this table is required, as it's still a variable!
        let t = mk_table_with(fun, ctxt);
        ctxt.locals.last_mut().unwrap().insert(String::from(*fn_ident), t);
    }
}

fn print_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    // TODO consider iterating over the table to print everything.
    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args_str = ctxt.push_compute(ir::Expr::Str(String::from("args")));
    let retval_str = ctxt.push_compute(ir::Expr::Str(String::from("retval")));
    let args = ctxt.push_compute(ir::Expr::Index(arg, args_str));
    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one));
    let (is_fn, call_node) = mk_fn_check(arg1, ctxt);

    let if_body = vec![
        ir::Statement::Compute(mk_node(ctxt), ir::Expr::Intrinsic(ir::Intrinsic::Print(call_node)))
    ];
    let else_body = vec![
        ir::Statement::Compute(mk_node(ctxt), ir::Expr::Intrinsic(ir::Intrinsic::Print(arg1)))
    ];
    ctxt.push_st(ir::Statement::If(is_fn, if_body, else_body));

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.zero, ctxt.zero);
    ctxt.push_store(arg, retval_str, ret);
    ctxt.push_st(ir::Statement::Return);
}

fn type_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    let function_str = ctxt.push_compute(ir::Expr::Str("function".to_string()));

    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args_str = ctxt.push_compute(ir::Expr::Str(String::from("args")));
    let retval_str = ctxt.push_compute(ir::Expr::Str(String::from("retval")));
    let args = ctxt.push_compute(ir::Expr::Index(arg, args_str));
    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one));
    let val = ctxt.push_compute(ir::Expr::Intrinsic(ir::Intrinsic::Type(arg1)));
    let (is_fn, _) = mk_fn_check(arg1, ctxt);

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.zero, ctxt.one);
    let if_body = vec![
        ir::Statement::Store(ret, ctxt.one, function_str)
    ];
    let else_body = vec![
        ir::Statement::Store(ret, ctxt.one, val)
    ];
    ctxt.push_st(ir::Statement::If(is_fn, if_body, else_body));
    
    ctxt.push_store(arg, retval_str, ret);
    ctxt.push_st(ir::Statement::Return);
}

fn next_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args_str = ctxt.push_compute(ir::Expr::Str(String::from("args")));
    let retval_str = ctxt.push_compute(ir::Expr::Str(String::from("retval")));
    let args = ctxt.push_compute(ir::Expr::Index(arg, args_str));
    let two = mk_num(2, ctxt);

    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one));
    mk_assert(mk_proper_table_check(arg1, ctxt), "Argument to next is not a table!", ctxt);
    let arg2 = ctxt.push_compute(ir::Expr::Index(args, two));
    let new_index = ctxt.push_compute(ir::Expr::Intrinsic(ir::Intrinsic::Next(arg1, arg2)));
    let new_val = ctxt.push_compute(ir::Expr::Index(arg1, new_index));

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.zero, two);
    ctxt.push_store(ret, ctxt.one, new_index);
    ctxt.push_store(ret, two, new_val);
    
    ctxt.push_store(arg, retval_str, ret);
    ctxt.push_st(ir::Statement::Return);
}

fn pairs_native_fn(ctxt: &mut Ctxt, native_impls: &NativeImpls) {
    let args_str = ctxt.push_compute(ir::Expr::Str(String::from("args")));
    let upvalues_str = ctxt.push_compute(ir::Expr::Str(String::from("upvalues")));
    let call_str = ctxt.push_compute(ir::Expr::Str(String::from("call")));
    let retval_str = ctxt.push_compute(ir::Expr::Str(String::from("retval")));

    let two = mk_num(2, ctxt);
    let three = mk_num(3, ctxt);

    let arg = ctxt.push_compute(ir::Expr::Arg);
    let args = ctxt.push_compute(ir::Expr::Index(arg, args_str));
    let arg1 = ctxt.push_compute(ir::Expr::Index(args, ctxt.one));

    let next_table = mk_table(ctxt);
    let tmp = mk_table(ctxt);
    ctxt.push_store(next_table, upvalues_str, tmp);

    let next_fn = ctxt.push_compute(ir::Expr::LitFunction(native_impls["next"]));
    ctxt.push_store(next_table, call_str, next_fn);

    let ret = mk_table(ctxt);
    ctxt.push_store(ret, ctxt.zero, three);

    ctxt.push_store(ret, ctxt.one, next_table);
    ctxt.push_store(ret, two, arg1);
    let nil_node = ctxt.push_compute(ir::Expr::Nil);
    ctxt.push_store(ret, three, nil_node);
    
    ctxt.push_store(arg, retval_str, ret);
    ctxt.push_st(ir::Statement::Return);
}
