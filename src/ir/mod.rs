mod lower;

#[derive(Debug, Clone)]
pub struct Function {
    body: Vec<Statement>,

    /// the argument names are merely debug information.
    args: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, Expr),
    FunctionCall(Expr, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct IR {
    main_idx: usize, // the IR::fns index of the implicit main function
    fns: Vec<Function>,

    /// the variable Var::Global(i) has the ident global_idents[i] in the original source file.
    /// this is only debug information.
    global_idents: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Var {
    Global(usize),
    FnArg(usize),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    LiteralNum(f64),
    Function(usize), // the usize indexes into IR::fns
    Plus(Box<Expr>, Box<Expr>),
}
