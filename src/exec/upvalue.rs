use super::*;

fn lvalue_candidates(lvalue: &LValue, mut cvars: HashSet<String>) -> HashSet<String> {
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
        Expr::Ellipsis => {},
        Expr::Literal(lit) => todo!(),
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
        Expr::FunctionCall(call) => function_call_candidates(call, cvars.clone()),
    }

    ret
}

fn function_call_candidates(call: &FunctionCall, mut cvars: HashSet<String>) -> HashSet<String> {
    todo!()
}

// finds the variables used without declaration in a list of statements
// those variables are candidates to be closured then.
// cvars = variables that could be closured by the function.
// candidates() is only allowed to return a subset of cvars.
pub(super) fn candidates(statements: &[Statement], mut cvars: HashSet<String>) -> HashSet<String> {
    for stmt in statements {
        /*
        Statement::Assign(Vec<LValue>, Vec<Expr>),
        Statement::FunctionCall(FunctionCall),
        Statement::While(Expr, /*body: */ Vec<Statement>),
        Statement::Repeat(/*body: */ Vec<Statement>, Expr),
        Statement::NumericFor(/*ident: */String, /*start: */Expr, /*stop: */Expr, /*step: */Option<Expr>, /*body: */ Vec<Statement>),
        Statement::GenericFor(Vec<String>, Vec<Expr>, /*body: */ Vec<Statement>),

        // each if and elseif corresponds to a entry in the Vec<IfBlock>
        Statement::If(Vec<IfBlock>, /*else-body: */ Option<Vec<Statement>>),
        Statement::Local(/*vars: */ Vec<String>, /*rhs: */ Vec<Expr>),
        Statement::Return(Vec<Expr>),
        Statement::Block(Vec<Statement>),
        Statement::Break => {},
        */
    }
    todo!()
}


