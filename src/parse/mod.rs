use crate::ast::*;
use crate::token::*;

mod statement;
use statement::parse_statement;

mod expr;

pub fn parse(tokens: &[Token]) -> Result<Ast, ()> {
    let (body, tokens) = parse_body(tokens)?;

    if !tokens.is_empty() {
        println!("parsing failed at: {:?}", tokens);
        Err(())
    } else {
        let ast = Ast { statements: body };
        Ok(ast)
    }
}

// this function parses as many statements as possible, so a syntax error just yields an incomplete Vec
// of statements. It needs to be checked when using this function!
pub fn parse_body(mut tokens: &[Token]) -> Result<(Vec<Statement>, &[Token]), ()> {
    let mut statements = Vec::new();
    while let Ok((stmt, ts)) = parse_statement(tokens) {
        tokens = ts;
        statements.push(stmt);
    }

    Ok((statements, tokens))
}
