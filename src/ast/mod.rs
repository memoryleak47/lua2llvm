mod token;
use token::*;

mod assemble;
use assemble::assemble_statement;

mod expr;
use expr::assemble_expr;

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Plus, Minus, Mul, Div, Mod,
    And, Or,
    Lt, Le, Gt, Ge,
    IsEqual, IsNotEqual,
    Concat, Pow,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String), LiteralNum(f64),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    Function {
        args: Vec<String>,
        body: Vec<Statement>,
    },
    FunctionCall {
        func: Box<Expr>,
        args: Vec<Expr>
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
    Return(Expr),
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
