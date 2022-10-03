use crate::ir::{self, IR};
use crate::ast::{self, Ast};

impl IR {
    pub fn lower(ast: &Ast) -> IR {
        let mut ir = IR {
            fns: Vec::new(),
            main_idx: 0,
            global_idents: Vec::new()
        };

        ir.main_idx = ir.lower_fn(&[], &ast.statements);
        ir
    }

    // returns the IR::fns index of the function
    fn lower_fn(&mut self, active_args: &[String], body: &[ast::Statement]) -> usize {
        // active_fn & active_args are named that way as this is the current function that's actively being lowered.
        let mut active_fn = ir::Function {
            body: Vec::new(),
            args: active_args.iter().cloned().collect()
        };

        for st in body {
            match st {
                ast::Statement::FunctionCall { func, args } => {
                    let lfunc = self.lower_expr(func, active_args);
                    let largs = args.iter().map(|x| self.lower_expr(x, active_args)).collect();
                    let stat = ir::Statement::FunctionCall(lfunc, largs);
                    active_fn.body.push(stat);
                },
                ast::Statement::Assign { var, expr } => {
                    let lvar = self.lower_var(var, active_args);
                    let lexpr = self.lower_expr(expr, active_args);
                    let stat = ir::Statement::Assign(lvar, lexpr);
                    active_fn.body.push(stat);
                },
                ast::Statement::Return(expr) => {
                    let lexpr = self.lower_expr(expr, active_args);
                    let stat = ir::Statement::Return(lexpr);
                    active_fn.body.push(stat);
                },
            }
        }

        let fn_idx = self.fns.len();
        self.fns.push(active_fn);
        fn_idx
    }

    fn lower_expr(&mut self, expr: &ast::Expr, active_args: &[String]) -> ir::Expr {
        match expr {
            ast::Expr::Var(x) => ir::Expr::Var(self.lower_var(x, active_args)),
            ast::Expr::LiteralNum(x) => ir::Expr::LiteralNum(*x as f64),
            ast::Expr::BinOp(kind, l, r) =>
                ir::Expr::BinOp(
                    *kind,
                    Box::new(self.lower_expr(l, active_args)),
                    Box::new(self.lower_expr(r, active_args)),
                ),
            ast::Expr::Function { args, body } => 
                ir::Expr::Function(self.lower_fn(args, body)),
            ast::Expr::FunctionCall { func, args } => {
                let lfunc = self.lower_expr(func, active_args);
                let largs: Vec<_> = args.iter().map(|expr| self.lower_expr(expr, active_args)).collect();
                ir::Expr::FunctionCall(Box::new(lfunc), largs)
            },
        }
    }

    fn lower_var(&mut self, x: &str, active_args: &[String]) -> ir::Var {
        if let Some(i) = active_args.iter().position(|y| y == x) {
            return ir::Var::FnArg(i);
        }

        if let Some(i) = self.global_idents.iter().position(|y| y == x) {
            return ir::Var::Global(i);
        }

        let i = self.global_idents.len();
        self.global_idents.push(x.to_string());
        ir::Var::Global(i)
    }
}
