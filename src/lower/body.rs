use crate::lower::*;

fn lower_if(ifblocks: &[IfBlock], optelse: &Option<Vec<Statement>>, ctxt: &mut Ctxt) {
    assert!(ifblocks.len() > 0);

    let post_bid = ctxt.alloc_block();

    for IfBlock(cond, ifbody) in ifblocks {
        ctxt.push_scope();
        let then_bid = ctxt.alloc_block();
        let else_bid = ctxt.alloc_block();

        let cond = lower_expr1(&cond, ctxt);
        ctxt.push_truthy_if(cond, then_bid, else_bid);

        ctxt.set_active_block(then_bid);
        lower_body(ifbody, ctxt);
        ctxt.push_goto(post_bid);

        ctxt.set_active_block(else_bid);
        ctxt.pop_scope();
    }

    if let Some(else_b) = optelse {
        ctxt.push_scope();
        lower_body(else_b, ctxt);
        ctxt.pop_scope();
    }

    ctxt.push_goto(post_bid);

    ctxt.set_active_block(post_bid);
}

pub(in crate::lower) fn lower_body(statements: &[Statement], ctxt: &mut Ctxt) {
    for st in statements {
        // If someone used `break` / `return`, it's over.
        if ctxt.fcx().active_block.is_none() {
            break;
        }

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
                    lvalues.push((n, ctxt.inner_str()));
                }
                lower_assign(&lvalues, exprs, ctxt);
                let map: &mut HashMap<_, _> = ctxt.fcx_mut().locals.last_mut().unwrap();
                for (var, (n, _)) in vars.iter().zip(lvalues.iter()) {
                    map.insert(var.clone(), *n);
                }
            },
            Statement::FunctionCall(call) => { lower_fn_call(call, ctxt); },
            Statement::Return(exprs) => {
                let t = table_wrap_exprlist(exprs, None, ctxt);
                return lower_return(t, ctxt);
            },
            Statement::Break => {
                ctxt.push_goto(*ctxt.fcx().break_bid_stack.last().expect("Cannot use `break` outside of loop"));
            }
            Statement::While(cond, body) => {
                let loop_start = ctxt.alloc_block();
                let loop_body = ctxt.alloc_block();
                let loop_post = ctxt.alloc_block();

                ctxt.push_goto(loop_start);

                ctxt.set_active_block(loop_start);
                let cond = lower_expr1(cond, ctxt);
                ctxt.push_truthy_if(cond, loop_body, loop_post);

                ctxt.set_active_block(loop_body);
                ctxt.fcx_mut().break_bid_stack.push(loop_post);
                ctxt.push_scope();
                lower_body(body, ctxt);
                ctxt.push_goto(loop_start);
                ctxt.pop_scope();
                ctxt.fcx_mut().break_bid_stack.pop();

                ctxt.set_active_block(loop_post);
            },
            Statement::If(ifblocks, optelse) => { lower_if(ifblocks, optelse, ctxt); }
            Statement::Block(body) => {
                ctxt.push_scope();
                lower_body(body, ctxt);
                ctxt.pop_scope();
            },
            Statement::NumericFor(..) => unreachable!(),
            Statement::GenericFor(..) => unreachable!(),
            Statement::Repeat(..) => unreachable!(),
        }
    }
}

pub(in crate::lower) fn lower_return(/*the table we want to return*/ ret: Node, ctxt: &mut Ctxt) {
    if !ctxt.is_main() {
        let arg = ctxt.push_compute(ir::Expr::Arg);
        ctxt.push_store(arg, ctxt.retval_str(), ret);
    }

    ctxt.push_st(ir::Statement::Return);
    ctxt.fcx_mut().active_block = None;
}


fn lower_assign(lvalues: &[(/*table: */ Node, /*index: */ Node)], exprs: &[Expr], ctxt: &mut Ctxt) {
    let mut exprs: Vec<Expr> = exprs.to_vec();

    // if exprs == [], just set everything to Nil!
    if exprs.is_empty() {
        let nil = ctxt.push_compute(ir::Expr::Nil);
        for (t, idx) in lvalues.iter() {
            ctxt.push_store(*t, *idx, nil);
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
        ctxt.push_store(*t, *i, *r);
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
        ctxt.push_store(t, idx, r);
    }
}
