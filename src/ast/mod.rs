mod token;

type Var = String;

enum Expr {
    Var(Var),
    LiteralNum(u32),
    Plus(Box<Expr>, Box<Expr>),
}

enum Statement {
    Print(Expr),
    Assignment(Var, Expr)
}

pub struct Ast {
    statements: Vec<Statement>,
}

impl Ast {
    fn parse(code: &str) -> Result<Ast, ()> {
        let tokens = token::tokenize(code);
        Err(())
    }
}
