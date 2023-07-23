use crate::ast::*;

pub fn prepare(ast: &mut Ast) {
    prepare_stmts(&mut ast.statements, &mut var_generator());
}

fn prepare_stmt(stmt: &mut Statement, gen: &mut VarGenerator) {
    match stmt {
        // statements to be resolved..
        Statement::GenericFor(names, exprs, block) => {
            *stmt = resolve_generic_for(names, exprs, block, gen);
            prepare_stmt(stmt, gen);
        },
        Statement::Repeat(body, until) => {
            *stmt = resolve_repeat(body, until);
            prepare_stmt(stmt, gen);
        },
        Statement::NumericFor(ident, start, stop, step, body) => {
            *stmt = resolve_numeric_for(ident, start, stop, step, body, gen);
            prepare_stmt(stmt, gen);
        },

        // the other statements..
        Statement::Block(stmts2) => {
            prepare_stmts(stmts2, gen);
        },
        Statement::If(ifblocks, opt_else) => {
            for IfBlock(cond, x) in ifblocks {
                prepare_expr(cond, gen);
                prepare_stmts(x, gen);
            }
            for x in opt_else {
                prepare_stmts(x, gen);
            }
        },
        Statement::While(cond, body) => {
            prepare_expr(cond, gen);
            prepare_stmts(body, gen);
        }

        Statement::Local(_, rhs) => {
            for e in rhs {
                prepare_expr(e, gen);
            }
        },
        Statement::Assign(lvals, exprs) => {
            for l in lvals {
                prepare_lvalue(l, gen);
            }
            for e in exprs {
                prepare_expr(e, gen);
            }
        },
        Statement::Return(exprs) => {
            for e in exprs {
                prepare_expr(e, gen);
            }
        },
        Statement::FunctionCall(fc) => prepare_function_call(fc, gen),
        Statement::Break => {},
    }
}

fn prepare_stmts(stmts: &mut Vec<Statement>, gen: &mut VarGenerator) {
    for s in stmts {
        prepare_stmt(s, gen);
    }
}

fn prepare_field(f: &mut Field, gen: &mut VarGenerator) {
    match f {
        Field::Expr(e1) => prepare_expr(e1, gen),
        Field::ExprToExpr(e1, e2) => {
            prepare_expr(e1, gen);
            prepare_expr(e2, gen);
        },
        Field::NameToExpr(_, e2) => prepare_expr(e2, gen),
    }
}

fn prepare_literal(literal: &mut Literal, gen: &mut VarGenerator) {
    match literal {
        Literal::Function(_, _, body) => {
            prepare_stmts(body, gen);
        },
        Literal::Table(fields) => {
            for f in fields {
                prepare_field(f, gen);
            }
        },
        _ => {},
    }
}

fn prepare_lvalue(lvalue: &mut LValue, gen: &mut VarGenerator) {
    match lvalue {
        LValue::Var(_) => {},
        LValue::Dot(e1, _) => {
            prepare_expr(e1, gen);
        },
        LValue::Index(e1, e2) => {
            prepare_expr(e1, gen);
            prepare_expr(e2, gen);
        },
    }
}

fn prepare_expr(expr: &mut Expr, gen: &mut VarGenerator) {
    match expr {
        Expr::Ellipsis => {},
        Expr::Literal(lit) => prepare_literal(lit, gen),
        Expr::LValue(lval) => prepare_lvalue(lval, gen),
        Expr::BinOp(_, l, r) => {
            prepare_expr(l, gen);
            prepare_expr(r, gen);
        },
        Expr::UnOp(_, r) => prepare_expr(r, gen),
        Expr::FunctionCall(fc) => prepare_function_call(fc, gen),
    }
}

fn prepare_function_call(fc: &mut FunctionCall, gen: &mut VarGenerator) {
    match fc {
        FunctionCall::Direct(f, args) => {
            prepare_expr(f, gen);
            for a in args {
                prepare_expr(a, gen);
            }
        },
        FunctionCall::Colon(table, _, args) => {
            prepare_expr(table, gen);
            for a in args {
                prepare_expr(a, gen);
            }
        }
    }
}

fn resolve_generic_for(names: &[String], exprs: &[Expr], block: &[Statement], gen: &mut VarGenerator) -> Statement {
    let (f, s, var) = (gen(), gen(), gen());
    let var2expr = |v: &str| Expr::LValue(Box::new(LValue::Var(v.to_string())));

    let mut while_stmts = vec![
        Statement::Local(names.iter().cloned().collect(), vec![Expr::FunctionCall(Box::new(FunctionCall::Direct(var2expr(&f), vec![var2expr(&s), var2expr(&var)])))]),
        Statement::Assign(vec![LValue::Var(var.to_string())], vec![var2expr(&names[0])]),
        Statement::If(vec![IfBlock(
            Expr::BinOp(BinOpKind::IsEqual, Box::new(var2expr(&var)), Box::new(Expr::Literal(Literal::Nil))),
            vec![Statement::Break],
        )], None),
    ];
    while_stmts.extend(block.iter().cloned());
    
    Statement::Block(vec![
        Statement::Local(vec![f, s, var], exprs.iter().cloned().collect()),
        Statement::While(Expr::Literal(Literal::Bool(true)), while_stmts),
    ])
}

fn resolve_repeat(body: &Vec<Statement>, until: &Expr) -> Statement {
    let mut body = body.clone();
    body.push(Statement::If(vec![IfBlock(until.clone(), vec![Statement::Break])], None));
    Statement::While(Expr::Literal(Literal::Bool(true)), body)
}

fn resolve_numeric_for(ident: &str, start_expr: &Expr, stop_expr: &Expr, step_expr: &Option<Expr>, body: &Vec<Statement>, gen: &mut VarGenerator) -> Statement {
    // TODO include error checks like Lua does.
    // Lua uses `tonumber` and then checks the outputs of that. I currently just assume that the args are really numbers.
    let (var, limit, step) = (gen(), gen(), gen());
    let str2lval = |v: &str| LValue::Var(v.to_string());
    let str2expr = |v: &str| Box::new(Expr::LValue(Box::new(str2lval(v))));

    let mut l = vec![start_expr.clone(), stop_expr.clone()];
    if let Some(x) = step_expr {
        l.push(x.clone());
    } else {
        l.push(Expr::Literal(Literal::Num(1.0)));
    }

    let while_cond = Expr::BinOp(BinOpKind::Or,
        Box::new(Expr::BinOp(BinOpKind::And,
            Box::new(Expr::BinOp(BinOpKind::Gt, str2expr(&step), Box::new(Expr::Literal(Literal::Num(0.0))))),
            Box::new(Expr::BinOp(BinOpKind::Le, str2expr(&var), str2expr(&limit))),
        )),
        Box::new(Expr::BinOp(BinOpKind::And,
            Box::new(Expr::BinOp(BinOpKind::Le, str2expr(&step), Box::new(Expr::Literal(Literal::Num(0.0))))),
            Box::new(Expr::BinOp(BinOpKind::Ge, str2expr(&var), str2expr(&limit))),
        )),
    );
    let mut while_body = body.clone();
    while_body.insert(0, Statement::Local(vec![ident.to_string()], vec![*str2expr(&var)]));
    while_body.push(Statement::Assign(vec![str2lval(&var)], vec![Expr::BinOp(BinOpKind::Plus, str2expr(&var), str2expr(&step))]));

    Statement::Block(vec![
        Statement::Local(vec![var, limit, step], l),
        Statement::While(while_cond, while_body),
    ])
}

type VarGenerator = impl FnMut() -> String;

// Generates variables that the programmer cannot access, hence they are collision-free.
fn var_generator() -> VarGenerator {
    let mut i = 0;
    return move || {
        let mut x = i.to_string();
        x.insert(0, '$');
        i += 1;
        x
    };
}
