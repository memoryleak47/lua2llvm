mod token;
use token::*;

mod assemble;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String), LiteralNum(u32),
    Plus(Box<Expr>, Box<Expr>),
    Function {
        args: Vec<String>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionCall {
        func: Expr,
        args: Vec<Expr>,
    },
    Assign {
        var: String,
        expr: Expr,
    },
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

impl Ast {
    pub fn parse(code: &str) -> Result<Ast, ()> {
        let tokens = token::tokenize(code);
        assemble::assemble(&tokens)
    }
}
