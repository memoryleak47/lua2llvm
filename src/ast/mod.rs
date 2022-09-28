mod token;
mod parse;

use token::Token;

type Var = String;

#[derive(Debug)]
enum Expr {
    Var(Var),
    LiteralNum(u32),
    Plus(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Statement {
    Print(Expr),
    Assignment(Var, Expr)
}

#[derive(Debug)]
pub struct Ast {
    statements: Vec<Statement>,
}
