use crate::lower::*;

pub(in crate::lower) fn lower_fn(args: &[String], variadic: &Variadic, statements: &[Statement], is_main: bool, ctxt: &mut Ctxt) -> (FnId, Vec<String>) {
    add_fn(|ctxt| {
        ctxt.is_main = is_main;

        // global environment functions
        if is_main {
            add_native_fns(ctxt);
        }

        if !args.is_empty() || *variadic == Variadic::Yes {
            // function args
            let arg = ctxt.push_compute(ir::Expr::Arg);
            let args_str = ctxt.push_compute(ir::Expr::Str("args".to_string()));
            let argtable = ctxt.push_compute(ir::Expr::Index(arg, args_str));

            for (i, arg) in args.iter().enumerate() {
                let t = mk_table(ctxt);
                // lua tables start with 1, not 0.
                let i = mk_num((i+1) as f64, ctxt);
                let val = ctxt.push_compute(ir::Expr::Index(argtable, i));
                ctxt.push_store(t, ctxt.one, val);

                ctxt.locals.last_mut().unwrap().insert(arg.clone(), t);
            }

            if *variadic == Variadic::Yes {
                // ARG_LEN = args.len()
                // ARGT_LEN = argtable[0]
                // E_LEN = ARGT_LEN - ARG_LEN -- length of ellipsis `...`
                // n = {}
                // n[0] <- E_LEN
                // i = 1
                // loop {
                //   if i > E_LEN: break
                //   n[i] = argtable[i+ARG_LEN]
                //   i = i + 1
                // }
                let arg_len = mk_num(args.len() as f64, ctxt);
                let argt_len = ctxt.push_compute(ir::Expr::Index(argtable, ctxt.zero));
                let e_len = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::Minus, argt_len, arg_len));
                let n = mk_table(ctxt);
                ctxt.push_store(n, ctxt.zero, e_len);

                let i = mk_table_with(ctxt.one, ctxt);

                let loopbody = ctxt.in_block(|ctxt| {
                    // if i > E_LEN: break
                    let i_node = ctxt.push_compute(ir::Expr::Index(i, ctxt.one));
                    let i_gt_e_len = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::Gt, i_node, e_len));
                    let then_body = ctxt.in_block(|ctxt| ctxt.push_st(ir::Statement::Break));
                    let else_body = ctxt.empty_block();
                    ctxt.push_st(ir::Statement::If(i_gt_e_len, then_body, else_body));

                    // n[i] = argtable[i+ARG_LEN]
                    let i_plus_arg_len = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, arg_len));
                    let argtable_indexed = ctxt.push_compute(ir::Expr::Index(argtable, i_plus_arg_len));
                    ctxt.push_store(n, i_node, argtable_indexed);

                    // i = i+1
                    let i_plus_one = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, ctxt.one));
                    ctxt.push_store(i, ctxt.one, i_plus_one);
                });
                ctxt.push_st(ir::Statement::Loop(loopbody));

                ctxt.ellipsis_node = Some(n);
            }
        }

        lower_body(statements, ctxt);

        // add `return` if missing
        if !matches!(ctxt.body.last(), Some(ir::Statement::Return)) {
            let t = mk_table(ctxt);
            ctxt.push_store(t, ctxt.zero, ctxt.zero);
            lower_return(t, ctxt);
        }

        ctxt.upvalue_idents.clone()
    }, ctxt)
}

// creates a new function and readies the ctxt, such that one can start adding the function in the callback.
pub(in crate::lower) fn add_fn<T>(callback: impl FnOnce(&mut Ctxt) -> T, ctxt: &mut Ctxt) -> (FnId, T) {
    let fid = ctxt.ir.fns.len();

    // this dummy allows us to have a fixed id before lowering of this fn is done.
    // this is necessary eg. for closuring.
    let dummy_lit_fn = LitFunction {
        body: Vec::new(),
    };
    ctxt.ir.fns.push(dummy_lit_fn);

    // TODO the following vars are pretty much a whole Ctxt, why not simply create a Ctxt directly?
    let mut locals = vec![HashMap::new()];
    let mut body = Vec::new();
    let mut next_node = 0;
    let mut upvalue_idents: Vec<String> = Vec::new();
    let mut ellipsis_node = None;
    let mut zero = usize::MAX;
    let mut one = usize::MAX;
    let mut is_main = false;

    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.upvalue_idents, &mut upvalue_idents);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);
    std::mem::swap(&mut ctxt.zero, &mut zero);
    std::mem::swap(&mut ctxt.one, &mut one);
    std::mem::swap(&mut ctxt.is_main, &mut is_main);

    ctxt.zero = mk_num(0.0, ctxt);
    ctxt.one = mk_num(1.0, ctxt);

    let t = callback(ctxt);

    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.upvalue_idents, &mut upvalue_idents);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);
    std::mem::swap(&mut ctxt.zero, &mut zero);
    std::mem::swap(&mut ctxt.one, &mut one);
    std::mem::swap(&mut ctxt.is_main, &mut is_main);

    ctxt.ir.fns[fid] = LitFunction { body };

    (fid, t)
}

// result is always tabled = true.
pub(in crate::lower) fn lower_fn_call(call: &FunctionCall, ctxt: &mut Ctxt) -> Node {
    let call_str = ctxt.push_compute(ir::Expr::Str(String::from("call")));
    let upvalues_str = ctxt.push_compute(ir::Expr::Str(String::from("upvalues")));
    let args_str = ctxt.push_compute(ir::Expr::Str(String::from("args")));
    let retval_str = ctxt.push_compute(ir::Expr::Str(String::from("retval")));

    match call {
        // f(x, y, z) --> f["call"]({"upvalues": f["upvalues"], "args": {[0]=3, x, y, z}})
        FunctionCall::Direct(f, args) => {
            let f = lower_expr1(f, ctxt);
            let f_call = ctxt.push_compute(ir::Expr::Index(f, call_str));

            let arg = mk_table(ctxt);

            let args = table_wrap_exprlist(args, None, ctxt);
            ctxt.push_store(arg, args_str, args);

            let upvalues = ctxt.push_compute(ir::Expr::Index(f, upvalues_str));
            ctxt.push_store(arg, upvalues_str, upvalues);

            ctxt.push_st(ir::Statement::FnCall(f_call, arg));

            ctxt.push_compute(ir::Expr::Index(arg, retval_str))
        },
        // obj:f(x, y, z) --> t[idx]["call"]({"upvalues": t[idx]["upvalues"], "args": {[0]=4, obj, x, y, z}})
        FunctionCall::Colon(t, idx, args) => {
            let t = lower_expr1(t, ctxt);

            let idx = ir::Expr::Str(idx.clone());
            let idx = ctxt.push_compute(idx);

            let f = ctxt.push_compute(ir::Expr::Index(t, idx));
            let f_call = ctxt.push_compute(ir::Expr::Index(f, call_str));

            let arg = mk_table(ctxt);

            let args = table_wrap_exprlist(args, Some(t), ctxt);
            ctxt.push_store(arg, args_str, args);

            let upvalues = ctxt.push_compute(ir::Expr::Index(f, upvalues_str));
            ctxt.push_store(arg, upvalues_str, upvalues);

            ctxt.push_st(ir::Statement::FnCall(f_call, arg));
            ctxt.push_compute(ir::Expr::Index(arg, retval_str))
        },
    }
}
