use crate::lower::*;

fn lower_if(ifblocks: &[IfBlock], optelse: &Option<Vec<Statement>>, ctxt: &mut Ctxt) {
    assert!(ifblocks.len() > 0);

    let IfBlock(cond, ifbody) = ifblocks[0].clone();
    let cond = lower_expr1(&cond, ctxt);

    let ifbody = ctxt.in_block(|ctxt| {
        lower_body(&ifbody, ctxt);
    });

    let elsebody = ctxt.in_block(|ctxt| {
        if ifblocks.len() == 1 {
            if let Some(else_b) = optelse {
                lower_body(else_b, ctxt);
            }
        } else { // recursion!
            lower_if(&ifblocks[1..], optelse, ctxt);
        }
    });
    ctxt.push_st(ir::Statement::If(cond, ifbody, elsebody));
}

pub(in crate::lower) fn lower_body(statements: &[Statement], ctxt: &mut Ctxt) {
    for st in statements {
        match st {
            Statement::Assign(lvalues, exprs) => {
                let lvalues: Vec<(Node, Node)> = lvalues.iter()
                                             .map(|lval| lower_lvalue(lval, ctxt))
                                             .collect();
                lower_assign(&lvalues, exprs, ctxt);
            },
            Statement::Local(vars, exprs) => {
                let mut lvalues: Vec<(Node, Node)> = Vec::new();
                for _ in vars.iter() {
                    let n = mk_table(ctxt);
                    lvalues.push((n, ctxt.one));
                }
                lower_assign(&lvalues, exprs, ctxt);
                let map: &mut HashMap<_, _> = ctxt.locals.last_mut().unwrap();
                for (var, (n, _)) in vars.iter().zip(lvalues.iter()) {
                    map.insert(var.clone(), *n);
                }
            },
            Statement::FunctionCall(call) => { lower_fn_call(call, ctxt); },
            Statement::Return(exprs) => {
                let t = table_wrap_exprlist(exprs, None, ctxt);
                lower_return(t, ctxt);
            },
            Statement::Break => {
                ctxt.push_st(ir::Statement::Break);
            }
            Statement::While(cond, body) => {
                let body = ctxt.in_block(|ctxt| {
                    let cond = lower_expr1(cond, ctxt);
                    ctxt.push_st(ir::Statement::If(cond, vec![], vec![ir::Statement::Break]));

                    lower_body(body, ctxt);
                });

                ctxt.push_st(ir::Statement::Loop(body));
            },
            Statement::If(ifblocks, optelse) => { lower_if(ifblocks, optelse, ctxt); }
            Statement::Block(body) => {
                let body = ctxt.in_block(|ctxt| {
                    lower_body(body, ctxt);
                });
                ctxt.body.extend(body);
            },
            Statement::NumericFor(..) => unreachable!(),
            Statement::GenericFor(..) => unreachable!(),
            Statement::Repeat(..) => unreachable!(),
        }
    }
}

pub(in crate::lower) fn lower_return(/*the table we want to return*/ ret: Node, ctxt: &mut Ctxt) {
    if !ctxt.is_main {
        let retval_str = ctxt.push_compute(ir::Expr::Str("retval".to_string()));
        let arg = ctxt.push_compute(ir::Expr::Arg);
        ctxt.push_st(ir::Statement::Store(arg, retval_str, ret));
    }
    ctxt.push_st(ir::Statement::Return);
}


fn lower_assign(lvalues: &[(/*table: */ Node, /*index: */ Node)], exprs: &[Expr], ctxt: &mut Ctxt) {
    let mut exprs: Vec<Expr> = exprs.to_vec();

    // if exprs == [], just set everything to Nil!
    if exprs.is_empty() {
        let nil = ctxt.push_compute(ir::Expr::Nil);
        for (t, idx) in lvalues.iter() {
            ctxt.push_st(ir::Statement::Store(*t, *idx, nil));
        }

        return;
    }

    // otherwise, last expr needs to handled differently.
    // if it's tabled, it needs to be unwrapped!
    let last = exprs.pop().unwrap();

    // non-tabled rhs nodes
    let mut rnodes: Vec<Node> = exprs.iter()
                     .map(|x| lower_expr1(x, ctxt))
                     .collect();
    let (last, tabled) = lower_expr(&last, ctxt);
    if !tabled {
        rnodes.push(last);
    }
    for ((t, i), r) in lvalues.iter().zip(rnodes.iter()) {
        ctxt.push_st(ir::Statement::Store(*t, *i, *r));
    }
    let min = rnodes.len();
    let max = lvalues.len();
    for i in min..max {
        let expr = if tabled {
            let i = i - min + 1; // starting at 1
            let x = mk_num(i as f64, ctxt);
            let x = ir::Expr::Index(last, x);

            x
        } else {
            ir::Expr::Nil
        };
        let r = ctxt.push_compute(expr);
        let (t, idx) = lvalues[i].clone();
        ctxt.push_st(ir::Statement::Store(t, idx, r));
    }
}
