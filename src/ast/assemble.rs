use super::*;

pub(super) fn assemble(mut tokens: &[Token]) -> Result<Ast, ()> {
    let mut ast = Ast { statements: vec![] };

    while !tokens.is_empty() {
        let (st, rest) = assemble_statement(tokens)?;
        tokens = rest;
        ast.statements.push(st);
    }

    Ok(ast)
}

// assemble_statement

pub(super) fn assemble_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    Err(())
        .or_else(|_| assemble_assign_statement(tokens))
        .or_else(|_| assemble_function_call_statement(tokens))
        .or_else(|_| assemble_return_statement(tokens))
}

fn assemble_function_call_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let (Expr::FunctionCall { func, args }, tokens) = assemble_expr(tokens)? else { return Err(()) };
    let stmt = Statement::FunctionCall { func: *func, args };
    Ok((stmt, tokens))
}

fn assemble_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Ident(var), tokens@..] = tokens else { return Err(()) };
    let [Token::Equals, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = assemble_expr(tokens)?;
    let stmt = Statement::Assign { var: var.to_string(), expr };
    Ok((stmt, tokens))
}

fn assemble_return_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Return, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = assemble_expr(tokens)?;
    let stmt = Statement::Return(expr);
    Ok((stmt, tokens))
}
