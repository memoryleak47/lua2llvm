use crate::ast::*;

// simplify attempts to:
// - resolve loops into "while 'true' + if + break" form
// - make each function return one table
// - make each function accept one table as argument
// - resolve ... functions by the above
// - resolve multi-assignments
// - unnest expressions
// - and & or are resolved to if
// - a.b & a:f() are resolved to indexing

// constraints:
// - Assign(ls, rs): ls.len() == 1 && rs.len() == 1.
// - Assign(ls, rs): each recursive Expr within ls / rs is a a local variable.
// - Assign(ls, rs): each recursive Expr within ls / rs is a a local variable.
// - Local(ls, rs): rs.len() == 0 && ls.len() == 1.
// - FunctionCall is FunctionCall::Direct(l, [r]) where both l and r are local variables.

struct Ctxt {
    id_counter: usize,
}

fn simplify_assign_statement(ls: &[LValue], rs: &[Expr], ctxt: &mut Ctxt) -> Vec<Statement> {
    let mut statements = Vec::new();

    let mut ls2 = Vec::new();
    for l in ls {
        let (l2, sts) = simplify_lvalue(l, ctxt);
        statements.extend(sts);
        ls2.push(l2);
    }

    // rs2 are the boring single (i.e. non-tabled) rhs values.
    let mut rs2 = Vec::new();
    for r in rs[..rs.len()-1].iter() {
        let (r2, sts) = simplify_expr1(r, ctxt);
        statements.extend(sts);
        rs2.push(r2);
    }

    let last = rs.last().unwrap();
    let (last, tabled, sts) = simplify_expr(last, ctxt);
    statements.extend(sts);

    if !tabled {
        rs2.push(last.clone());
    }

    for (lval, rval) in ls2.iter().zip(rs2.iter()) {
        let st = Statement::Assign(vec![lval.clone()], vec![rval.clone()]);
        statements.push(st);
    }
    let min = rs2.len();
    let max = ls2.len();
    for idx in min..max {
        let expr = if tabled {
                let x = idx - min + 1; // starting with 1
                let x = Expr::Literal(Literal::Num(x as f64));
                let x = LValue::Index(last.clone(), x);
                let x = Expr::LValue(Box::new(x));
                let (x, sts) = simplify_expr1(&x, ctxt);
                statements.extend(sts);

                x
            } else {
                Expr::Literal(Literal::Nil)
            };
        let st = Statement::Assign(vec![ls2[idx].clone()], vec![expr]);
        statements.push(st);
    }

    statements
}

fn mk_tmp(stmts: &mut Vec<Statement>, ctxt: &mut Ctxt) -> LValue {
    let s = format!("${}", ctxt.id_counter);

    let st = Statement::Local(vec![s.clone()], Vec::new());
    stmts.push(st);

    ctxt.id_counter += 1;

    LValue::Var(s)
}

fn simplify_statement(st: &Statement, ctxt: &mut Ctxt) -> Vec<Statement> {
    let mut statements = Vec::new();
    match st {
        Statement::Break => {
            statements.push(Statement::Break);
        },
        Statement::Assign(ls, rs) => {
            statements.extend(simplify_assign_statement(ls, rs, ctxt));
        },
        Statement::Local(vars, rs) => {
            let mut tmps = Vec::new();
            for _ in vars {
                tmps.push(mk_tmp(&mut statements, ctxt));
            }
            statements.extend(simplify_assign_statement(&tmps, rs, ctxt));
            for (i, l) in vars.iter().enumerate() {
                let st = Statement::Local(vec![l.clone()], Vec::new());
                statements.push(st);
                let lval = LValue::Var(l.clone());
                let expr = Expr::LValue(Box::new(tmps[i].clone()));
                let st = Statement::Assign(vec![lval], vec![expr]);
                statements.push(st);
            }
        },
        Statement::FunctionCall(call) => {
            todo!()
        },
        _ => todo!(),
    /*
        Statement::While(Expr, /*body: */ Vec<Statement>),
        Statement::Repeat(/*body: */ Vec<Statement>, Expr),
        Statement::NumericFor(/*ident: */String, /*start: */Expr, /*stop: */Expr, /*step: */Option<Expr>, /*body: */ Vec<Statement>),
        Statement::GenericFor(Vec<String>, Vec<Expr>, /*body: */ Vec<Statement>),
        Statement::If(Vec<IfBlock>, /*else-body: */ Option<Vec<Statement>>),
        Statement::Return(Vec<Expr>),
        Statement::Block(Vec<Statement>),
    */
    }

    statements

}

fn simplify_lvalue(lvalue: &LValue, ctxt: &mut Ctxt) -> (LValue, Vec<Statement>) {
    let mut statements = Vec::new();
    let lval = match lvalue {
        LValue::Var(x) => LValue::Var(x.clone()),
        LValue::Dot(expr, field) => {
            let (l, sts) = simplify_expr1(expr, ctxt);
            statements.extend(sts);
            let r = Expr::Literal(Literal::Str(field.clone()));
            let (r, sts) = simplify_expr1(&r, ctxt);
            statements.extend(sts);

            LValue::Index(l, r)
        },
        LValue::Index(l, r) => {
            let (l, sts) = simplify_expr1(l, ctxt);
            statements.extend(sts);
            let (r, sts) = simplify_expr1(r, ctxt);
            statements.extend(sts);

            LValue::Index(l, r)
        },
    };
    (lval, statements)
}

// like simplify_expr but autmatically unwraps the wrapper table if "tabled = true" using t[1].
fn simplify_expr1(expr: &Expr, ctxt: &mut Ctxt) -> (Expr, Vec<Statement>) {
    let (mut expr, tabled, mut statements) = simplify_expr(expr, ctxt);

    if tabled {
        let idx = Expr::Literal(Literal::Num(1 as f64));
        let e = Expr::LValue(Box::new(LValue::Index(expr, idx)));
        let (e, tabled2, sts) = simplify_expr(&e, ctxt);
        assert_eq!(tabled2, false);

        statements.extend(sts);
        expr = e;
    }

    (expr, statements)
}

// will yield a newly created local variable, containing the computed result of expr.
// if expr is a multi-argument object, like ... or a function call, this expr has been tabled,
// and the bool "tabled" will be true.
fn simplify_expr(expr: &Expr, ctxt: &mut Ctxt) -> (Expr, /*tabled: */ bool, Vec<Statement>) {
    let mut statements = Vec::new();
    let mut tabled = false;

    let expr = match expr {
        expr@Expr::Literal(Literal::Num(i)) => expr.clone(),
        Expr::LValue(lvalue) => {
            let (lvalue, sts) = simplify_lvalue(lvalue, ctxt);
            statements.extend(sts);

            Expr::LValue(Box::new(lvalue))
        },
        _ => todo!(),
/*
        Expr::Ellipsis,
        Expr::BinOp(BinOpKind, /*l: */ Box<Expr>, /*r: */ Box<Expr>),
        Expr::UnOp(UnOpKind, /*r: */ Box<Expr>),
        Expr::FunctionCall(Box<FunctionCall>),
*/
    };

    let l = mk_tmp(&mut statements, ctxt);
    let st = Statement::Assign(vec![l.clone()], vec![expr]);
    statements.push(st);

    let e = Expr::LValue(Box::new(l));
    (e, tabled, statements)
}

pub fn simplify(ast: &Ast) -> Ast {
    let mut ret = Ast { statements: Vec::new() };
    let mut ctxt = Ctxt { id_counter: 0 };
    for st in &ast.statements {
        ret.statements.extend(simplify_statement(st, &mut ctxt));
    }

    ret
}
