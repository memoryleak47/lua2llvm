use crate::lower::*;

type NativeImpls = HashMap<&'static str, FnId>;
static NATIVE_FNS: &'static [(&'static str, fn(&mut Ctxt, &NativeImpls))] = &[("print", print_native_fn), ("next", next_native_fn), ("type", type_native_fn), ("pairs", pairs_native_fn)];

// ctxt is currently implementing main at this point.
pub(in crate::lower) fn add_native_fns(ctxt: &mut Ctxt) {
    let mut native_impls: NativeImpls = HashMap::new();

    let call_str = mk_compute(ir::Expr::Str(String::from("call")), ctxt);
    let upvalues_str = mk_compute(ir::Expr::Str(String::from("upvalues")), ctxt);

    for (fn_ident, generator) in NATIVE_FNS.iter() {
        let (fn_id, ()) = add_fn(|ctxt| generator(ctxt, &native_impls), ctxt);
        native_impls.insert(fn_ident, fn_id);

        let fun = mk_table(ctxt);

        let upvalues = mk_table(ctxt);
        push_st(ir::Statement::Store(fun, upvalues_str, upvalues), ctxt);

        let call = mk_compute(ir::Expr::LitFunction(fn_id), ctxt);
        push_st(ir::Statement::Store(fun, call_str, call), ctxt);

        // this table is required, as it's still a variable!
        let t = mk_table_with(fun, ctxt);
        ctxt.locals.last_mut().unwrap().insert(String::from(*fn_ident), t);
    }
}

fn print_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    // TODO consider iterating over the table to print everything.
    let arg = mk_compute(ir::Expr::Arg, ctxt);
    let args_str = mk_compute(ir::Expr::Str(String::from("args")), ctxt);
    let args = mk_compute(ir::Expr::Index(arg, args_str), ctxt);
    let arg1 = mk_compute(ir::Expr::Index(args, ctxt.one), ctxt);
    let (is_fn, call_node) = mk_fn_check(arg1, ctxt);

    let if_body = vec![
        ir::Statement::Compute(mk_node(ctxt), ir::Expr::Intrinsic(ir::Intrinsic::Print(call_node)))
    ];
    let else_body = vec![
        ir::Statement::Compute(mk_node(ctxt), ir::Expr::Intrinsic(ir::Intrinsic::Print(arg1)))
    ];
    push_st(ir::Statement::If(is_fn, if_body, else_body), ctxt);

    let ret = mk_table(ctxt);
    push_st(ir::Statement::Store(ret, ctxt.zero, ctxt.zero), ctxt);
    push_st(ir::Statement::Return(ret), ctxt);
}

fn type_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    let function_str = mk_compute(ir::Expr::Str("function".to_string()), ctxt);

    let arg = mk_compute(ir::Expr::Arg, ctxt);
    let args_str = mk_compute(ir::Expr::Str(String::from("args")), ctxt);
    let args = mk_compute(ir::Expr::Index(arg, args_str), ctxt);
    let arg1 = mk_compute(ir::Expr::Index(args, ctxt.one), ctxt);
    let val = mk_compute(ir::Expr::Intrinsic(ir::Intrinsic::Type(arg1)), ctxt);
    let (is_fn, _) = mk_fn_check(arg1, ctxt);

    let ret = mk_table(ctxt);
    push_st(ir::Statement::Store(ret, ctxt.zero, ctxt.one), ctxt);
    let if_body = vec![
        ir::Statement::Store(ret, ctxt.one, function_str)
    ];
    let else_body = vec![
        ir::Statement::Store(ret, ctxt.one, val)
    ];
    push_st(ir::Statement::If(is_fn, if_body, else_body), ctxt);
    
    push_st(ir::Statement::Return(ret), ctxt);
}

fn next_native_fn(ctxt: &mut Ctxt, _native_impls: &NativeImpls) {
    let arg = mk_compute(ir::Expr::Arg, ctxt);
    let args_str = mk_compute(ir::Expr::Str(String::from("args")), ctxt);
    let args = mk_compute(ir::Expr::Index(arg, args_str), ctxt);
    let two = mk_num(2, ctxt);

    let arg1 = mk_compute(ir::Expr::Index(args, ctxt.one), ctxt);
    mk_assert(mk_proper_table_check(arg1, ctxt), "Argument to next is not a table!", ctxt);
    let arg2 = mk_compute(ir::Expr::Index(args, two), ctxt);
    let new_index = mk_compute(ir::Expr::Intrinsic(ir::Intrinsic::Next(arg1, arg2)), ctxt);
    let new_val = mk_compute(ir::Expr::Index(arg1, new_index), ctxt);

    let ret = mk_table(ctxt);
    push_st(ir::Statement::Store(ret, ctxt.zero, two), ctxt);
    push_st(ir::Statement::Store(ret, ctxt.one, new_index), ctxt);
    push_st(ir::Statement::Store(ret, two, new_val), ctxt);
    
    push_st(ir::Statement::Return(ret), ctxt);
}

fn pairs_native_fn(ctxt: &mut Ctxt, native_impls: &NativeImpls) {
    let args_str = mk_compute(ir::Expr::Str(String::from("args")), ctxt);
    let upvalues_str = mk_compute(ir::Expr::Str(String::from("upvalues")), ctxt);
    let call_str = mk_compute(ir::Expr::Str(String::from("call")), ctxt);

    let two = mk_num(2, ctxt);
    let three = mk_num(3, ctxt);

    let arg = mk_compute(ir::Expr::Arg, ctxt);
    let args = mk_compute(ir::Expr::Index(arg, args_str), ctxt);
    let arg1 = mk_compute(ir::Expr::Index(args, ctxt.one), ctxt);

    let next_table = mk_table(ctxt);
    push_st(ir::Statement::Store(next_table, upvalues_str, mk_table(ctxt)), ctxt);

    let next_fn = mk_compute(ir::Expr::LitFunction(native_impls["next"]), ctxt);
    push_st(ir::Statement::Store(next_table, call_str, next_fn), ctxt);

    let ret = mk_table(ctxt);
    push_st(ir::Statement::Store(ret, ctxt.zero, three), ctxt);

    push_st(ir::Statement::Store(ret, ctxt.one, next_table), ctxt);
    push_st(ir::Statement::Store(ret, two, arg1), ctxt);
    let nil_node = mk_compute(ir::Expr::Nil, ctxt);
    push_st(ir::Statement::Store(ret, three, nil_node), ctxt);
    
    push_st(ir::Statement::Return(ret), ctxt);
}
