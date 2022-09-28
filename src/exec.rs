use std::collections::HashMap;

use crate::ast::*;

impl Ast {
    pub fn exec(&self) {
        let mut vars: HashMap<String, u32> = HashMap::new();

        for st in &self.statements {
            exec_statement(st, &mut vars);
        }
    }
}

fn exec_statement(stmt: &Statement, vars: &mut HashMap<String, u32>) {
    match stmt {
        Statement::Print(expr) => {
            println!("{}", eval_expr(expr, vars));
        },
        Statement::Assignment(var, expr) => {
            vars.insert(var.clone(), eval_expr(expr, vars));
        }
    }
}

fn eval_expr(expr: &Expr, vars: &HashMap<String, u32>) -> u32 {
    match expr {
        Expr::LiteralNum(x) => *x,
        Expr::Var(var) => *vars.get(var).expect("variable not defined!"),
        Expr::Plus(a, b) => eval_expr(a, vars) + eval_expr(b, vars),
    }
}
