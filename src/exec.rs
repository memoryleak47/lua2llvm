use crate::*;

use std::collections::HashMap;

#[derive(Default)]
struct Exec {
    vars: HashMap<String, u32>,
}

pub fn exec(ast: &Ast) {
    Exec::new().exec(ast)
}

impl Exec {
    fn new() -> Exec {
        Default::default()
    }

    fn exec(&mut self, ast: &Ast) {
        for st in &ast.statements {
            self.exec_statement(st);
        }
    }

    fn exec_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::FunctionCall { fn_name, args } => {
                if fn_name == "print" {
                    for arg in args {
                        println!("{}", self.eval_expr(arg));
                    }
                } else { todo!() }
            },
            Statement::Assign { var, expr } => {
                self.vars.insert(var.clone(), self.eval_expr(expr));
            }
            Statement::FunctionDef { .. } => todo!(),
        }
    }

    fn eval_expr(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::LiteralNum(x) => *x,
            Expr::Var(var) => *self.vars.get(var).expect("variable not defined!"),
            Expr::Plus(a, b) => self.eval_expr(a) + self.eval_expr(b),
        }
    }
}
