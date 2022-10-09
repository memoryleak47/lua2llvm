#![allow(unused)]

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
// - Return(exprs): exprs.len() == 1 && exprs == [local variable]
// - FunctionCall is FunctionCall::Direct(l, [r]) where both l and r are local variables.
// - Expr::Literal(Literal::Table(fields)): fields.len() == 0

struct Ctxt {
    id_counter: usize,
}

fn simplify_assign_statement(ls: &[LValue], rs: &[Expr], statements: &mut Vec<Statement>, ctxt: &mut Ctxt) {
    let mut ls2 = Vec::new();
    for l in ls {
        ls2.push(simplify_lvalue(l, statements, ctxt));
    }

    // rs2 are the boring single (i.e. non-tabled) rhs values.
    let mut rs2 = Vec::new();
    for r in rs[..rs.len()-1].iter() {
        rs2.push(simplify_expr1(r, statements, ctxt));
    }

    let last = rs.last().unwrap();
    let (last, tabled) = simplify_expr(last, statements, ctxt);

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
                let x = simplify_expr1(&x, statements, ctxt);

                x
            } else {
                Expr::Literal(Literal::Nil)
            };
        let st = Statement::Assign(vec![ls2[idx].clone()], vec![expr]);
        statements.push(st);
    }
}

fn mk_tmp(stmts: &mut Vec<Statement>, ctxt: &mut Ctxt) -> LValue {
    let s = format!("${}", ctxt.id_counter);

    let st = Statement::Local(vec![s.clone()], Vec::new());
    stmts.push(st);

    ctxt.id_counter += 1;

    LValue::Var(s)
}

fn simplify_statement(st: &Statement, statements: &mut Vec<Statement>, ctxt: &mut Ctxt) {
    match st {
        Statement::Break => {
            statements.push(Statement::Break);
        },
        Statement::Assign(ls, rs) => simplify_assign_statement(ls, rs, statements, ctxt),
        Statement::Local(vars, rs) => {
            let mut tmps = Vec::new();
            for _ in vars {
                tmps.push(mk_tmp(statements, ctxt));
            }
            simplify_assign_statement(&tmps, rs, statements, ctxt);
            for (i, l) in vars.iter().enumerate() {
                let st = Statement::Local(vec![l.clone()], Vec::new());
                statements.push(st);
                let lval = LValue::Var(l.clone());
                let expr = Expr::LValue(Box::new(tmps[i].clone()));
                let st = Statement::Assign(vec![lval], vec![expr]);
                statements.push(st);
            }
        },
        Statement::FunctionCall(FunctionCall::Direct(l, args)) => {
            let l = simplify_expr1(l, statements, ctxt);
            let fields: Vec<_> = args.iter().cloned().map(Field::Expr).collect();
            let r = Expr::Literal(Literal::Table(fields));
            let r = simplify_expr1(&r, statements, ctxt);

            let st = Statement::FunctionCall(FunctionCall::Direct(l, vec![r]));
            statements.push(st);
        },
        Statement::Return(exprs) => {
            let fields: Vec<_> = exprs.iter().cloned().map(Field::Expr).collect();
            let ret = Expr::Literal(Literal::Table(fields));
            let ret = simplify_expr1(&ret, statements, ctxt);
            let st = Statement::Return(vec![ret]);
            statements.push(st);
        },
        _ => todo!(),
    /*
        Statement::While(Expr, /*body: */ Vec<Statement>),
        Statement::Repeat(/*body: */ Vec<Statement>, Expr),
        Statement::NumericFor(/*ident: */String, /*start: */Expr, /*stop: */Expr, /*step: */Option<Expr>, /*body: */ Vec<Statement>),
        Statement::GenericFor(Vec<String>, Vec<Expr>, /*body: */ Vec<Statement>),
        Statement::If(Vec<IfBlock>, /*else-body: */ Option<Vec<Statement>>),
        Statement::Block(Vec<Statement>),
    */
    }
}

fn simplify_lvalue(lvalue: &LValue, statements: &mut Vec<Statement>, ctxt: &mut Ctxt) -> LValue {
    let lval = match lvalue {
        LValue::Var(x) => LValue::Var(x.clone()),
        LValue::Dot(expr, field) => {
            let l = simplify_expr1(expr, statements, ctxt);
            let r = Expr::Literal(Literal::Str(field.clone()));
            let r = simplify_expr1(&r, statements, ctxt);

            LValue::Index(l, r)
        },
        LValue::Index(l, r) => {
            let l = simplify_expr1(l, statements, ctxt);
            let r = simplify_expr1(r, statements, ctxt);

            LValue::Index(l, r)
        },
    };
    lval
}

// like simplify_expr but autmatically unwraps the wrapper table if "tabled = true" using t[1].
fn simplify_expr1(expr: &Expr, statements: &mut Vec<Statement>, ctxt: &mut Ctxt) -> Expr {
    let (mut expr, tabled) = simplify_expr(expr, statements, ctxt);

    if tabled {
        let idx = Expr::Literal(Literal::Num(1 as f64));
        let e = Expr::LValue(Box::new(LValue::Index(expr, idx)));
        let (e, tabled2) = simplify_expr(&e, statements, ctxt);
        assert_eq!(tabled2, false);

        expr = e;
    }

    expr
}

// will yield a newly created local variable, containing the computed result of expr.
// if expr is a multi-argument object, like ... or a function call, this expr has been tabled,
// and the bool "tabled" will be true.
fn simplify_expr(expr: &Expr, statements: &mut Vec<Statement>, ctxt: &mut Ctxt) -> (Expr, /*tabled: */ bool) {
    let mut tabled = false;

    let expr = match expr {
        expr@Expr::Literal(Literal::Num(i)) => expr.clone(),
        Expr::LValue(lvalue) => {
            let lvalue = simplify_lvalue(lvalue, statements, ctxt);

            Expr::LValue(Box::new(lvalue))
        },
        Expr::Literal(Literal::Table(fields)) => {
            todo!() // TODO
        },
        _ => todo!(),
/*
        Expr::Ellipsis,
        Expr::BinOp(BinOpKind, /*l: */ Box<Expr>, /*r: */ Box<Expr>),
        Expr::UnOp(UnOpKind, /*r: */ Box<Expr>),
        Expr::FunctionCall(Box<FunctionCall>),
*/
    };

    let l = mk_tmp(statements, ctxt);
    let st = Statement::Assign(vec![l.clone()], vec![expr]);
    statements.push(st);

    let e = Expr::LValue(Box::new(l));
    (e, tabled)
}

pub fn simplify(ast: &Ast) -> Ast {
    let mut ret = Ast { statements: Vec::new() };
    let mut ctxt = Ctxt { id_counter: 0 };
    for st in &ast.statements {
        simplify_statement(st, &mut ret.statements, &mut ctxt);
    }

    ret
}
