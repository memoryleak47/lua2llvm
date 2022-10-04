use crate::ast::*;
use crate::token::*;

mod statement;
use statement::parse_statement;

mod expr;

pub fn parse(mut tokens: &[Token]) -> Result<Ast, ()> {
    let mut ast = Ast { statements: vec![] };

    while !tokens.is_empty() {
        let (st, rest) = parse_statement(tokens)?;
        tokens = rest;
        ast.statements.push(st);
    }

    Ok(ast)
}
