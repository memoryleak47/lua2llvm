struct Function {
    body: Vec<Statement>,
    no_args: usize, // number of arguments FnArg(0) .. FnArg(no_args)
}

enum Statement {
    Assign(Var, Expr),
    FunctionCall(Var, Vec<Expr>),
}

struct IR {
    // fns[0] is always the main function
    fns: Vec<Function>,
}

enum Var {
    Global(usize),
    FnArg(usize),
}

enum Expr {
    Var(Var),
    LiteralNum(f64),
    Function(usize), // the usize indexes into IR::fns
    Plus(Box<Expr>, Box<Expr>),
}
