mod token;

use token::Token;

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
        Ast::assemble(&tokens)
    }

    fn assemble(mut tokens: &[Token]) -> Result<Ast, ()> {
        let mut ast = Ast { statements: vec![] };

        while !tokens.is_empty() {
            let (st, rest) = Ast::assemble_statement(tokens)?;
            tokens = rest;
            ast.statements.push(st);
        }

        Ok(ast)
    }

    fn assemble_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
        Ast::assemble_print_statement(tokens)
            .or_else(|_|
                Ast::assemble_assign_statement(tokens)
            )
    }

    fn assemble_print_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
        todo!() }
    fn assemble_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
        todo!() }
}
