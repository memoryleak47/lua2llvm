use crate::lower::*;

pub(in crate::lower) fn lower_fn(args: &[String], variadic: &Variadic, statements: &[Statement], is_main: bool, ctxt: &mut Ctxt) -> (FnId, Vec<String>) {
    add_fn(is_main, |ctxt| {
        // global environment functions
        if ctxt.is_main() {
            add_native_fns(ctxt);
        }

        if !args.is_empty() || *variadic == Variadic::Yes {
            // function args
            let arg = ctxt.push_compute(hir::Expr::Arg);
            let argtable = ctxt.push_compute(hir::Expr::Index(arg, ctxt.args_str()));

            for (i, arg) in args.iter().enumerate() {
                let t = mk_table(ctxt);
                // lua tables start with 1, not 0.
                let i = mk_num((i+1) as f64, ctxt);
                let val = ctxt.push_compute(hir::Expr::Index(argtable, i));
                ctxt.push_store(t, ctxt.inner_str(), val);

                ctxt.fcx_mut().locals.last_mut().unwrap().insert(arg.clone(), t);
            }

            if *variadic == Variadic::Yes {
                // ARG_LEN = args.len()
                // ARGT_LEN = argtable["count"]
                // E_LEN = ARGT_LEN - ARG_LEN -- length of ellipsis `...`
                // n = {}
                // n["count"] <- E_LEN
                // i = 1
                // loop {
                //   if i > E_LEN: break
                //   n[i] = argtable[i+ARG_LEN]
                //   i = i + 1
                // }
                let arg_len = mk_num(args.len() as f64, ctxt);
                let argt_len = ctxt.push_compute(hir::Expr::Index(argtable, ctxt.count_str()));
                let e_len = ctxt.push_compute(hir::Expr::BinOp(hir::BinOpKind::Minus, argt_len, arg_len));
                let n = mk_table(ctxt);
                ctxt.push_store(n, ctxt.count_str(), e_len);

                let i = mk_table_with(ctxt.one(), ctxt);

                let loop_start = ctxt.alloc_block();
                let loop_body = ctxt.alloc_block();
                let loop_post = ctxt.alloc_block();

                ctxt.push_goto(loop_start);
                ctxt.set_active_block(loop_start);

                // if i > E_LEN: break
                let i_node = ctxt.push_compute(hir::Expr::Index(i, ctxt.inner_str()));
                let i_gt_e_len = ctxt.push_compute(hir::Expr::BinOp(hir::BinOpKind::Gt, i_node, e_len));
                ctxt.push_if(i_gt_e_len, loop_post, loop_body);

                ctxt.set_active_block(loop_body);

                // n[i] = argtable[i+ARG_LEN]
                let i_plus_arg_len = ctxt.push_compute(hir::Expr::BinOp(hir::BinOpKind::Plus, i_node, arg_len));
                let argtable_indexed = ctxt.push_compute(hir::Expr::Index(argtable, i_plus_arg_len));
                ctxt.push_store(n, i_node, argtable_indexed);

                // i = i+1
                let i_plus_one = ctxt.push_compute(hir::Expr::BinOp(hir::BinOpKind::Plus, i_node, ctxt.one()));
                ctxt.push_store(i, ctxt.inner_str(), i_plus_one);
                ctxt.push_goto(loop_start);

                ctxt.set_active_block(loop_post);

                ctxt.fcx_mut().ellipsis_node = Some(n);
            }
        }

        lower_body(statements, ctxt);

        // add `return` if missing
        if ctxt.fcx().active_block.is_some() {
            let t = mk_table(ctxt);
            ctxt.push_store(t, ctxt.count_str(), ctxt.zero());
            lower_return(t, ctxt);
        }

        ctxt.fcx().upvalue_idents.clone()
    }, ctxt)
}

// creates a new function and readies the ctxt, such that one can start adding the function in the callback.
pub(in crate::lower) fn add_fn<T>(is_main: bool, callback: impl FnOnce(&mut Ctxt) -> T, ctxt: &mut Ctxt) -> (FnId, T) {
    let fid = ctxt.hir.fns.len();

    let mut lit_fn = Function {
        blocks: HashMap::new(),
        start_block: 0,
    };
    lit_fn.blocks.insert(0, vec![]);
    ctxt.hir.fns.insert(fid, lit_fn);

    if is_main {
        ctxt.hir.main_fn = fid;
    }

    let new_fcx = FnCtxt {
        locals: vec![HashMap::new()],
        next_node: 0,
        upvalue_idents: vec![],
        ellipsis_node: None,
        zero: usize::MAX,
        one: usize::MAX,
        true_: usize::MAX,
        retval_str: usize::MAX,
        call_str: usize::MAX,
        upvalues_str: usize::MAX,
        args_str: usize::MAX,
        table_str: usize::MAX,
        function_str: usize::MAX,
        count_str: usize::MAX,
        inner_str: usize::MAX,
        active_block: Some(0),
        init_block: 0,
        fn_id: fid,
        break_bid_stack: vec![],
    };
    ctxt.fn_stack.push(new_fcx);

    // currently active block = init_block
    ctxt.fcx_mut().zero = mk_num(0.0, ctxt);
    ctxt.fcx_mut().one = mk_num(1.0, ctxt);
    ctxt.fcx_mut().true_ = ctxt.push_compute(hir::Expr::Bool(true));
    ctxt.fcx_mut().retval_str = mk_str("retval", ctxt);
    ctxt.fcx_mut().call_str = mk_str("call", ctxt);
    ctxt.fcx_mut().upvalues_str = mk_str("upvalues", ctxt);
    ctxt.fcx_mut().args_str = mk_str("args", ctxt);
    ctxt.fcx_mut().table_str = mk_str("table", ctxt);
    ctxt.fcx_mut().function_str = mk_str("function", ctxt);
    ctxt.fcx_mut().count_str = mk_str("count", ctxt);
    ctxt.fcx_mut().inner_str = mk_str("inner", ctxt);

    let post_init_block = ctxt.alloc_block();
    ctxt.push_goto(post_init_block);
    ctxt.set_active_block(post_init_block);

    let t = callback(ctxt);

    ctxt.fn_stack.pop();

    (fid, t)
}

// result is always tabled = true.
pub(in crate::lower) fn lower_fn_call(call: &FunctionCall, ctxt: &mut Ctxt) -> Node {
    match call {
        // f(x, y, z) --> f["call"]({"upvalues": f["upvalues"], "args": {[0]=3, x, y, z}})
        FunctionCall::Direct(f, args) => {
            let f = lower_expr1(f, ctxt);
            let f_call = ctxt.push_compute(hir::Expr::Index(f, ctxt.call_str()));

            let arg = mk_table(ctxt);

            let args = table_wrap_exprlist(args, None, ctxt);
            ctxt.push_store(arg, ctxt.args_str(), args);

            let upvalues = ctxt.push_compute(hir::Expr::Index(f, ctxt.upvalues_str()));
            ctxt.push_store(arg, ctxt.upvalues_str(), upvalues);

            ctxt.push_st(hir::Statement::FnCall(f_call, arg));

            ctxt.push_compute(hir::Expr::Index(arg, ctxt.retval_str()))
        },
        // obj:f(x, y, z) --> t[idx]["call"]({"upvalues": t[idx]["upvalues"], "args": {[0]=4, obj, x, y, z}})
        FunctionCall::Colon(t, idx, args) => {
            let t = lower_expr1(t, ctxt);

            let idx = hir::Expr::Str(idx.clone());
            let idx = ctxt.push_compute(idx);

            let f = ctxt.push_compute(hir::Expr::Index(t, idx));
            let f_call = ctxt.push_compute(hir::Expr::Index(f, ctxt.call_str()));

            let arg = mk_table(ctxt);

            let args = table_wrap_exprlist(args, Some(t), ctxt);
            ctxt.push_store(arg, ctxt.args_str(), args);

            let upvalues = ctxt.push_compute(hir::Expr::Index(f, ctxt.upvalues_str()));
            ctxt.push_store(arg, ctxt.upvalues_str(), upvalues);

            ctxt.push_st(hir::Statement::FnCall(f_call, arg));
            ctxt.push_compute(hir::Expr::Index(arg, ctxt.retval_str()))
        },
    }
}
