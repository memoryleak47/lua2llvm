use crate::ast::*;

pub fn prepare(ast: &mut Ast) {
    prepare_stmts(&mut ast.statements, &mut var_generator());
}

fn prepare_stmts(stmts: &mut Vec<Statement>, gen: &mut VarGenerator) {
    let mut i = 0;
    // TODO also prepare within functions
    while i < stmts.len() {
        match &mut stmts[i] {
            // statements to be resolved..
            Statement::GenericFor(names, exprs, block) => {
                stmts[i] = resolve_generic_for(names, exprs, block, gen);
            },
            Statement::Repeat(body, until) => {
                stmts[i] = resolve_repeat(body, until);
            },
// TODO resolve these:
/*
            Statement::NumericFor(names, exprs, block) => *s = resolve_numeric_for(names, exprs, block),
*/

            // recursive statements
            Statement::Block(stmts2) => {
                prepare_stmts(stmts2, gen);
                i += 1;
            },
            Statement::If(ifblocks, opt_else) => {
                for IfBlock(_, x) in ifblocks {
                    prepare_stmts(x, gen);
                }
                for x in opt_else {
                    prepare_stmts(x, gen);
                }
                i += 1;
            },
            Statement::While(_, body) => {
                prepare_stmts(body, gen);
                i += 1;
            }

            // statements without a body..
            _ => { i += 1; },
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
