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
            let arg = mk_compute(ir::Expr::Arg, ctxt);
            let args_str = mk_compute(ir::Expr::Str("args".to_string()), ctxt);
            let argtable = mk_compute(ir::Expr::Index(arg, args_str), ctxt);

            for (i, arg) in args.iter().enumerate() {
                let t = mk_table(ctxt);
                // lua tables start with 1, not 0.
                let i = mk_num((i+1) as f64, ctxt);
                let val = mk_compute(ir::Expr::Index(argtable, i), ctxt);
                push_st(ir::Statement::Store(t, ctxt.one, val), ctxt);

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
                let argt_len = mk_compute(ir::Expr::Index(argtable, ctxt.zero), ctxt);
                let e_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Minus, argt_len, arg_len), ctxt);
                let n = mk_table(ctxt);
                push_st(ir::Statement::Store(n, ctxt.zero, e_len), ctxt);

                let i = mk_table_with(ctxt.one, ctxt);

                let loopbody = ctxt.in_block(|ctxt| {
                    // if i > E_LEN: break
                    let i_node = mk_compute(ir::Expr::Index(i, ctxt.one), ctxt);
                    let i_gt_e_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Gt, i_node, e_len), ctxt);
                    push_st(ir::Statement::If(i_gt_e_len, vec![ir::Statement::Break], vec![]), ctxt);

                    // n[i] = argtable[i+ARG_LEN]
                    let i_plus_arg_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, arg_len), ctxt);
                    let argtable_indexed = mk_compute(ir::Expr::Index(argtable, i_plus_arg_len), ctxt);
                    push_st(ir::Statement::Store(n, i_node, argtable_indexed), ctxt);

                    // i = i+1
                    let i_plus_one = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, ctxt.one), ctxt);
                    push_st(ir::Statement::Store(i, ctxt.one, i_plus_one), ctxt);
                });
                push_st(ir::Statement::Loop(loopbody), ctxt);

                ctxt.ellipsis_node = Some(n);
            }
        }

        lower_body(statements, ctxt);

        // add `return` if missing
        if !matches!(ctxt.body.last(), Some(ir::Statement::Return)) {
            let t = mk_table(ctxt);
            push_st(ir::Statement::Store(t, ctxt.zero, ctxt.zero), ctxt);
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

