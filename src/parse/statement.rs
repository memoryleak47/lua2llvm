use crate::parse::expr::parse_expr;
use crate::ast::*;
use crate::token::Token;

// parse_statement

pub(super) fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    Err(())
        .or_else(|_| parse_assign_statement(tokens))
        .or_else(|_| parse_local_statement(tokens))
        .or_else(|_| parse_function_call_statement(tokens))
        .or_else(|_| parse_return_statement(tokens))
        .or_else(|_| parse_break_statement(tokens))
}

fn parse_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Ident(var), tokens@..] = tokens else { return Err(()) };
    let [Token::Equals, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = parse_expr(tokens)?;
    let stmt = Statement::Assign(LValue::Var(var.to_string()), expr);
    Ok((stmt, tokens))
}

fn parse_local_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Local, Token::Ident(var), tokens@..] = tokens else { return Err(()) };

    let mut tokens = tokens;
    let mut opt: Option<Expr> = None;

    if let [Token::Equals, ts@..] = tokens {
        let (expr, ts) = parse_expr(ts)?;
        opt = Some(expr);
        tokens = ts;
    }

    let stmt = Statement::Local(var.clone(), opt);
    Ok((stmt, tokens))
}


fn parse_function_call_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let (Expr::FunctionCall(box FunctionCall::Direct(func, args)), tokens) = parse_expr(tokens)? else { return Err(()) };
    let stmt = Statement::FunctionCall(FunctionCall::Direct(func, args));
    Ok((stmt, tokens))
}


fn parse_return_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Return, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = parse_expr(tokens)?;
    let stmt = Statement::Return(expr);
    Ok((stmt, tokens))
}

fn parse_break_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Return, tokens@..] = tokens else { return Err(()) };
    let stmt = Statement::Break;
    Ok((stmt, tokens))
}
