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

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Neg, Hash, Not
}

#[derive(Debug, Clone)]
pub enum Field {
    Expr(Expr), // 15
    ExprToExpr(Expr, Expr), // [2+1] = 12
    NameToExpr(String, Expr), // name = "foo"
}

#[derive(Debug, Clone)]
pub enum Literal {
    Num(f64),
    Str(String),
    Bool(bool),
    Function(/*args: */Vec<String>, /*body: */ Vec<Statement>),
    Table(Vec<Field>),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(String),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
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
