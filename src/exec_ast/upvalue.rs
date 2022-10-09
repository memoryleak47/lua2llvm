use super::*;

fn lvalue_candidates(lvalue: &LValue, cvars: HashSet<String>) -> HashSet<String> {
    let mut ret: HashSet<String> = HashSet::new();
    match lvalue {
        LValue::Var(s) => {
            if cvars.contains(s) {
                ret.insert(s.clone());
            }
        },
        LValue::Dot(expr, _) => {
            ret.extend(expr_candidates(expr, cvars.clone()));
        },
        LValue::Index(e1, e2) => {
            ret.extend(expr_candidates(e1, cvars.clone()));
            ret.extend(expr_candidates(e2, cvars.clone()));
        },
    }

    ret
}

fn expr_candidates(expr: &Expr, mut cvars: HashSet<String>) -> HashSet<String> {
    let mut ret: HashSet<String> = HashSet::new();
    match expr {
        Expr::Ellipsis | Expr::Literal(Literal::Num(_) | Literal::Bool(_) | Literal::Str(_) | Literal::Nil) => {},
        Expr::Literal(Literal::Function(args, _variadic, body)) => {
            for arg in args {
                cvars.remove(arg);
            }
            ret.extend(candidates(body, cvars));
        },
        Expr::Literal(Literal::Table(fields)) => {
            for f in fields {
                match f {
                    Field::Expr(expr) => {
                        ret.extend(expr_candidates(expr, cvars.clone()));
                    },
                    Field::NameToExpr(_name, expr) => {
                        ret.extend(expr_candidates(expr, cvars.clone()));
                    },
                    Field::ExprToExpr(e1, e2) => {
                        ret.extend(expr_candidates(e1, cvars.clone()));
                        ret.extend(expr_candidates(e2, cvars.clone()));
                    },
                }
            }
        }
        Expr::LValue(lvalue) => {
            ret.extend(lvalue_candidates(&*lvalue, cvars.clone()));
        }
        Expr::BinOp(_, l, r) => {
            ret.extend(expr_candidates(&*l, cvars.clone()));
            ret.extend(expr_candidates(&*r, cvars.clone()));
        },
        Expr::UnOp(_, r) => {
            ret.extend(expr_candidates(&*r, cvars.clone()));
        },
        Expr::FunctionCall(call) => {
            ret.extend(function_call_candidates(call, cvars.clone()));
        },
    }

    ret
}

fn function_call_candidates(call: &FunctionCall, cvars: HashSet<String>) -> HashSet<String> {
    let mut exprs: Vec<&Expr> = Vec::new();
    match call {
        FunctionCall::Direct(expr, args) => {
            exprs.push(expr);
            exprs.extend(args.iter());
        },
        FunctionCall::Colon(expr, _func, args) => {
            exprs.push(expr);
            exprs.extend(args.iter());
        },
    }

    let mut ret: HashSet<String> = HashSet::new();
    for expr in exprs {
        ret.extend(expr_candidates(expr, cvars.clone()));
    }

    ret
}

// finds the variables used without declaration in a list of statements
// those variables are candidates to be closured then.
// cvars = variables that could be closured by the function.
// candidates() is only allowed to return a subset of cvars.
pub(super) fn candidates(statements: &[Statement], mut cvars: HashSet<String>) -> HashSet<String> {
    let mut ret: HashSet<String> = HashSet::new();
    for stmt in statements {
        match stmt {
            Statement::Assign(lvalues, exprs) => {
                for l in lvalues {
                    ret.extend(lvalue_candidates(l, cvars.clone()));
                }
                for e in exprs {
                    ret.extend(expr_candidates(e, cvars.clone()));
                }
            },
            Statement::FunctionCall(call) => {
                ret.extend(function_call_candidates(call, cvars.clone()));
            },
            Statement::Local(vars, rhs) => {
                for e in rhs {
                    ret.extend(expr_candidates(e, cvars.clone()));
                }
                for v in vars {
                    cvars.remove(v);
                }
                
            },
            Statement::Block(body) => {
                ret.extend(candidates(body, cvars.clone()));
            },
            Statement::While(expr, body) => {
                ret.extend(expr_candidates(expr, cvars.clone()));
                ret.extend(candidates(body, cvars.clone()));
            },
            Statement::Repeat(body, expr) => {
                ret.extend(expr_candidates(expr, cvars.clone()));
                ret.extend(candidates(body, cvars.clone()));
            },
            Statement::NumericFor(ident, start, stop, optstep, body) => {
                ret.extend(expr_candidates(start, cvars.clone()));
                ret.extend(expr_candidates(stop, cvars.clone()));
                if let Some(step) = optstep {
                    ret.extend(expr_candidates(step, cvars.clone()));
                }
                let mut cvars = cvars.clone();
                cvars.remove(ident);
                ret.extend(candidates(body, cvars));
            },
            Statement::GenericFor(names, exprs, body) => {
                for e in exprs {
                    ret.extend(expr_candidates(e, cvars.clone()));
                }
                let mut cvars = cvars.clone();
                for n in names {
                    cvars.remove(n);
                }
                ret.extend(candidates(body, cvars));
            },
            Statement::If(blocks, optelse) => {
                for IfBlock(cond, body) in blocks {
                    ret.extend(expr_candidates(cond, cvars.clone()));
                    ret.extend(candidates(body, cvars.clone()));
                }
                if let Some(body) = optelse {
                    ret.extend(candidates(body, cvars.clone()));
                }
            },
            Statement::Return(exprs) => {
                for e in exprs {
                    ret.extend(expr_candidates(e, cvars.clone()));
                }
            }
            Statement::Break => {},
        }
    }

    ret
}


