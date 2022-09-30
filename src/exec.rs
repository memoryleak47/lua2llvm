use crate::*;

use std::collections::HashMap;

pub fn exec(ast: &Ast) {
    let mut vars: HashMap<String, u32> = HashMap::new();

    for st in &ast.statements {
        exec_statement(st, &mut vars);
    }
}

fn exec_statement(stmt: &Statement, vars: &mut HashMap<String, u32>) {
    match stmt {
        Statement::FunctionCall { fn_name, args } => {
            if fn_name == "print" {
                for arg in args {
                    println!("{}", eval_expr(arg, vars));
                }
            } else { todo!() }
        },
        Statement::Assign { var, expr } => {
            vars.insert(var.clone(), eval_expr(expr, vars));
        }
        Statement::FunctionDef { .. } => todo!(),
    }
}

fn eval_expr(expr: &Expr, vars: &HashMap<String, u32>) -> u32 {
    match expr {
        Expr::LiteralNum(x) => *x,
        Expr::Var(var) => *vars.get(var).expect("variable not defined!"),
        Expr::Plus(a, b) => eval_expr(a, vars) + eval_expr(b, vars),
    }
}
