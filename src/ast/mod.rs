mod token;
pub use token::*;

mod parse;
pub use parse::*;

#[derive(Debug)]
pub enum Expr {
    Var(String),
    LiteralNum(u32),
    Plus(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Statement {
    FunctionCall {
        fn_name: String,
        args: Vec<Expr>,
    },
    FunctionDef {
        name: String,
        body: Vec<Ast>,
    },
    Assign {
        var: String,
        expr: Expr,
    },
}

#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}
