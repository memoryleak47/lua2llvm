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
    Neg, Len, Not
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
    BinOp(BinOpKind, /*l: */ Box<Expr>, /*r: */ Box<Expr>),
    UnOp(UnOpKind, /*l: */ Box<Expr>),
    FunctionCall(/*func: */ Box<Expr>, /*args: */ Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct ElseIf(/*condition: */ Expr, /*body: */ Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(/*var: */ String, Expr),
    FunctionCall(/*function: */ Expr, /*args: */ Vec<Expr>),
    While(Expr, /*body: */ Vec<Statement>),
    If(Expr, /*if-body: */ Vec<Statement>, /*list of else-ifs: */ Vec<ElseIf>, /*else-body: */ Option<Vec<Statement>>),
    Local(/*var: */ String, /*rhs: */ Option<Expr>),
    Return(Expr),
    Break,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

impl Ast {
    pub fn parse(code: &str) -> Result<Ast, ()> {
        let tokens = token::tokenize(code);
        dbg!(&tokens);
        assemble::assemble(&tokens)
    }
}
