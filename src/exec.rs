use crate::*;

use std::collections::HashMap;

#[derive(Default)]
struct StackFrame {
    vars: HashMap<String, u32>,
    statements: Vec<Statement>,
    next_statement: usize,
}

enum Function {
    Native(Box<dyn Fn(Vec<u32>)>),
    Lua {
        args: Vec<String>,
        statements: Vec<Statement>,
    }
}

#[derive(Default)]
struct Exec {
    stack: Vec<StackFrame>,
    fns: HashMap<String, Function>,
}

pub fn exec(ast: &Ast) {
    let mut e = Exec::default();
    e.stack.push(StackFrame {
        vars: Default::default(),
        statements: ast.statements.clone(),
        next_statement: 0,
    });
    let print = |vec: Vec<u32>| {
        for arg in vec {
            println!("{}", arg);
        }
    };
    e.fns.insert("print".to_string(), Function::Native(Box::new(print)));
    e.exec();
}

impl Exec {
    fn exec(&mut self) {
        while let Some(f) = self.stack.last_mut() {
            if f.next_statement < f.statements.len() {
                let stmt = f.statements[f.next_statement].clone();
                f.next_statement += 1;
                self.exec_statement(&stmt);
            } else {
                self.stack.pop();
            }
        }
    }

    fn exec_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::FunctionCall { fn_name, args } => {
                let val_args: Vec<u32> = args.iter().map(|expr| self.eval_expr(expr) ).collect();
                match &self.fns[fn_name] {
                    Function::Native(f) => f(val_args),
                    Function::Lua { args, statements } => {
                        let mut sf = StackFrame::default();
                        sf.statements = statements.clone();
                        for (name, value) in args.iter().zip(val_args.iter()) {
                            sf.vars.insert(name.clone(), *value);
                        }
                        self.stack.push(sf);
                    },
                }
            },
            Statement::Assign { var, expr } => {
                let val = self.eval_expr(expr);
                let frame: &mut StackFrame = match self.stack.iter_mut().rfind(|f| f.vars.contains_key(var)) {
                    Some(x) => x,
                    None => &mut self.stack[0],
                };
                frame.vars.insert(var.clone(), val);
            }
            Statement::FunctionDef { fn_name, args, body } => {
                let function = Function::Lua { args: args.clone(), statements: body.clone() };
                self.fns.insert(fn_name.clone(), function);
            }
        }
    }

    fn eval_expr(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::LiteralNum(x) => *x,
            Expr::Var(var) => {
                let frame: &StackFrame = self.stack.iter().rfind(|f| f.vars.contains_key(var)).expect("variable not defined!");
                *frame.vars.get(var).unwrap()
            }
            Expr::Plus(a, b) => self.eval_expr(a) + self.eval_expr(b),
        }
    }
}
