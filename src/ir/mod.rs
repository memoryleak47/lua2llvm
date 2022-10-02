pub struct Function {
    body: Vec<Statement>,
    no_args: usize, // number of arguments FnArg(0) .. FnArg(no_args)
}

pub enum Statement {
    Assign(Var, Expr),
    FunctionCall(Var, Vec<Expr>),
}

pub struct IR {
    // fns[0] is always the main function
    fns: Vec<Function>,
}

pub enum Var {
    Global(usize),
    FnArg(usize),
}

pub enum Expr {
    Var(Var),
    LiteralNum(f64),
    Function(usize), // the usize indexes into IR::fns
    Plus(Box<Expr>, Box<Expr>),
}
