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
    pub fn parse(code: &str) -> Result<Ast, ()> {
        let tokens = token::tokenize(code);
        dbg!(&tokens);
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
        let Token::Ident(print_ident) = &tokens[0] else { return Err(()) };
        if print_ident != "print" { return Err(()); }
        if tokens[1] != Token::LParen { return Err(()); }
        let (expr, rest) = Ast::assemble_expr(&tokens[2..])?;
        if rest[0] != Token::RParen { return Err(()); }
        let stmt = Statement::Print(expr);
        Ok((stmt, &rest[1..]))
    }

    fn assemble_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
        let Token::Ident(var) = &tokens[0] else { return Err(()) };
        if tokens[1] != Token::Equals { return Err(()); }
        let (expr, rest) = Ast::assemble_expr(&tokens[2..])?;
        let stmt = Statement::Assignment(var.to_string(), expr);
        Ok((stmt, rest))
    }

    fn assemble_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
        Ast::assemble_plus_expr(tokens)
            .or_else(|_|
                Ast::assemble_var_expr(tokens)
            )
    }

    fn assemble_var_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
        let Token::Ident(var) = &tokens[0] else { return Err(()) };
        let expr = Expr::Var(var.to_string());
        Ok((expr, &tokens[1..]))
    }

    fn assemble_plus_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
        todo!()
    }
}
