mod token;
pub use token::*;

mod parse;
pub use parse::*;

pub type Var = String;

#[derive(Debug)]
pub enum Expr {
    Var(Var),
    LiteralNum(u32),
    Plus(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Statement {
    Print(Expr),
    Assignment(Var, Expr)
}

#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}
