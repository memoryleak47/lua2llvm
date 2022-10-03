mod lower;

use crate::ast::BinOpKind;

#[derive(Debug, Clone)]
pub struct Function {
    pub body: Vec<Statement>,

    /// the argument names are merely debug information.
    #[allow(dead_code)]
    pub args: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, Expr),
    FunctionCall(Expr, Vec<Expr>),
    Return(Expr),
}

#[derive(Debug, Clone)]
pub struct IR {
    pub main_idx: usize, // the IR::fns index of the implicit main function
    pub fns: Vec<Function>,

    /// the variable Var::Global(i) has the ident global_idents[i] in the original source file.
    /// this is only debug information.
    #[allow(dead_code)]
    pub global_idents: Vec<String>,
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
    FunctionCall(Box<Expr>, Vec<Expr>),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
}
