use crate::lower::*;

// same as lower_expr, but does _[1] for "tabled = true" automatically.
pub(in crate::lower) fn lower_expr1(expr: &Expr, ctxt: &mut Ctxt) -> Node {
    let (n, tabled) = lower_expr(expr, ctxt);
    if tabled {
        let x = ir::Expr::Index(n, mk_num(1.0, ctxt));
        let x = ctxt.push_compute(x);

        x
    } else {
        n
    }
}


// "tabled" is true for function calls and ellipsis expressions.
// which return tables after transforming them.
pub(in crate::lower) fn lower_expr(expr: &Expr, ctxt: &mut Ctxt) -> (Node, /*tabled: */ bool) {
    let mut tabled = false;
    let node = match expr {
        Expr::Ellipsis => {
            tabled = true;

            ctxt.fcx().ellipsis_node.expect("lowering `...` in non-variadic function!")
        },
        Expr::Literal(Literal::Function(args, variadic, body)) => {
            let (fid, upvalue_idents) = lower_fn(args, variadic, body, false, ctxt);

            let n = mk_table(ctxt);

            let call = ctxt.push_compute(ir::Expr::LitFunction(fid));
            ctxt.push_store(n, ctxt.call_str(), call);

            let upvalues = mk_table(ctxt);
            for u in &upvalue_idents {
                let upvalue_ident = ctxt.push_compute(ir::Expr::Str(u.to_string()));
                let n = locate_ident(u, ctxt);
                ctxt.push_store(upvalues, upvalue_ident, n);
            }
            ctxt.push_store(n, ctxt.upvalues_str(), upvalues);

            n
        },
        Expr::Literal(Literal::Table(fields)) => {
            lower_table(fields, None, /*calc-length: */ false, ctxt)
        },
        Expr::LValue(lval) => {
            let (t, idx) = lower_lvalue(lval, ctxt);
            let x = ir::Expr::Index(t, idx);

            ctxt.push_compute(x)
        },
        Expr::BinOp(kind, l, r) => lower_binop(kind, l, r, ctxt),
        Expr::UnOp(kind, r) => lower_unop(kind, r, ctxt),
        Expr::FunctionCall(call) => {
            tabled = true;

            lower_fn_call(call, ctxt)
        },

        // literals
        Expr::Literal(Literal::Num(i)) => mk_num(*i as f64, ctxt),
        Expr::Literal(Literal::Bool(b)) => ctxt.push_compute(ir::Expr::Bool(*b)),
        Expr::Literal(Literal::Str(s)) => ctxt.push_compute(ir::Expr::Str(s.clone())),
        Expr::Literal(Literal::Nil) => ctxt.push_compute(ir::Expr::Nil),
    };

    (node, tabled)
}

fn lower_binop(kind: &BinOpKind, l: &Expr, r: &Expr, ctxt: &mut Ctxt) -> Node {
    let kind = match kind {
        BinOpKind::Plus => ir::BinOpKind::Plus,
        BinOpKind::Minus => ir::BinOpKind::Minus,
        BinOpKind::Mul => ir::BinOpKind::Mul,
        BinOpKind::Div => ir::BinOpKind::Div,
        BinOpKind::Mod => ir::BinOpKind::Mod,
        BinOpKind::Lt => ir::BinOpKind::Lt,
        BinOpKind::Le => ir::BinOpKind::Le,
        BinOpKind::Gt => ir::BinOpKind::Gt,
        BinOpKind::Ge => ir::BinOpKind::Ge,
        BinOpKind::IsEqual => ir::BinOpKind::IsEqual,
        BinOpKind::IsNotEqual => ir::BinOpKind::IsNotEqual,
        BinOpKind::Concat => ir::BinOpKind::Concat,
        BinOpKind::Pow => ir::BinOpKind::Pow,

        BinOpKind::And => {
            let l: Node = lower_expr1(l, ctxt);
            let t = mk_table_with(l, ctxt);

            let then_bid = ctxt.alloc_block();
            let post_bid = ctxt.alloc_block();

            ctxt.push_truthy_if(l, then_bid, post_bid);

            ctxt.set_active_block(then_bid);
            let r: Node = lower_expr1(r, ctxt);
            ctxt.push_store(t, ctxt.inner_str(), r);
            ctxt.push_goto(post_bid);

            ctxt.set_active_block(post_bid);

            return ctxt.push_compute(ir::Expr::Index(t, ctxt.inner_str()));
        },
        BinOpKind::Or => {
            let l: Node = lower_expr1(l, ctxt);
            let t = mk_table_with(l, ctxt);

            let else_bid = ctxt.alloc_block();
            let post_bid = ctxt.alloc_block();

            ctxt.push_truthy_if(l, post_bid, else_bid);

            ctxt.set_active_block(else_bid);
            let r: Node = lower_expr1(r, ctxt);
            ctxt.push_store(t, ctxt.inner_str(), r);
            ctxt.push_goto(post_bid);

            ctxt.set_active_block(post_bid);

            return ctxt.push_compute(ir::Expr::Index(t, ctxt.inner_str()));
        },
    };

    let l = lower_expr1(l, ctxt);
    let r = lower_expr1(r, ctxt);
    let x = ir::Expr::BinOp(kind, l, r);

    ctxt.push_compute(x)
}

fn lower_unop(kind: &UnOpKind, r: &Expr, ctxt: &mut Ctxt) -> Node {
    let r = lower_expr1(r, ctxt);

    let x = match kind {
        UnOpKind::Neg => ir::Expr::BinOp(ir::BinOpKind::Minus, ctxt.zero(), r),
        UnOpKind::Len => ir::Expr::Len(r),
        UnOpKind::Not => {
            let t = mk_table_with(ctxt.true_(), ctxt);

            let then_bid = ctxt.alloc_block();
            let post_bid = ctxt.alloc_block();

            ctxt.push_truthy_if(r, then_bid, post_bid);

            ctxt.set_active_block(then_bid);
            let false_v = ctxt.push_compute(ir::Expr::Bool(false));
            ctxt.push_store(t, ctxt.inner_str(), false_v);
            ctxt.push_goto(post_bid);

            ctxt.set_active_block(post_bid);

            ir::Expr::Index(t, ctxt.inner_str())
        },
    };

    ctxt.push_compute(x)

}

// returns the `Node` which stores the TablePtr to it.
pub(in crate::lower) fn locate_ident(s: &str, ctxt: &mut Ctxt) -> Node {
    for loc in ctxt.fcx().locals.iter().rev() {
        if let Some(n) = loc.get(s) {
            return *n;
        }
    }

    // if it's not defined in the locals, it has to be an upvalue!
    // or in-case of "main", it's just a local variable

    let new_n = ctxt.append_to_init_block(|ctxt| {
        if ctxt.is_main() {
            ctxt.push_compute(ir::Expr::NewTable)
        } else {
            let arg = ctxt.push_compute(ir::Expr::Arg);
            let upvalues_table = ctxt.push_compute(ir::Expr::Index(arg, ctxt.upvalues_str()));
            let upvalue_ident = ctxt.push_compute(ir::Expr::Str(s.to_string()));

            ctxt.push_compute(ir::Expr::Index(upvalues_table, upvalue_ident))
        }
    });

    ctxt.fcx_mut().locals[0].insert(s.to_string(), new_n); // upvalues need to be on the bottom of the stack!
    ctxt.fcx_mut().upvalue_idents.push(s.to_string());

    new_n
}

pub(in crate::lower) fn lower_lvalue(lvalue: &LValue, ctxt: &mut Ctxt) -> (/*table: */ Node, /*index*/ Node) {
    match lvalue {
        LValue::Var(s) => {
            let n = locate_ident(s, ctxt);
            return (n, ctxt.inner_str());
        },
        LValue::Dot(expr, field) => {
            let l = lower_expr1(expr, ctxt);
            let r = ctxt.push_compute(ir::Expr::Str(field.clone()));
            mk_assert(mk_proper_table_check(l, ctxt), "Trying to index into non-table!", ctxt);

            (l, r)
        },
        LValue::Index(l, r) => {
            let l = lower_expr1(l, ctxt);
            let r = lower_expr1(r, ctxt);
            mk_assert(mk_proper_table_check(l, ctxt), "Trying to index into non-table!", ctxt);

            (l, r)
        },
    }
}
