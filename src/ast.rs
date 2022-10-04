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
pub enum LValue {
    Var(String), // foo
    Dot(Expr, String), // foo.bar
    Index(Expr, Expr), // thingy[2]
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
pub enum FunctionCall {
    Direct(/*expr: */Expr, /*args: */ Vec<Expr>), // <expr>(<args>)
    Colon(/*expr: */ Expr, /*func: */ String, /*args: */Vec<Expr>), // <expr>:func(<args>)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    LValue(Box<LValue>),
    BinOp(BinOpKind, /*l: */ Box<Expr>, /*r: */ Box<Expr>),
    UnOp(UnOpKind, /*r: */ Box<Expr>),
    FunctionCall(Box<FunctionCall>),
}

#[derive(Debug, Clone)]
pub struct IfBlock(pub /*condition: */ Expr, pub /*body: */ Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(LValue, Expr),
    FunctionCall(FunctionCall),
    While(Expr, /*body: */ Vec<Statement>),

    // each if and elseif corresponds to a entry in the Vec<IfBlock>
    If(Vec<IfBlock>, /*else-body: */ Option<Vec<Statement>>),
    Local(/*var: */ String, /*rhs: */ Option<Expr>),
    Return(Expr),
    Break,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub statements: Vec<Statement>,
}
