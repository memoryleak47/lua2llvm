use crate::parse::{expr::parse_expr, parse_body};
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
        .or_else(|_| parse_while_statement(tokens))
        .or_else(|_| parse_if_statement(tokens))
}

fn parse_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let (Expr::LValue(lvalue), tokens) = parse_expr(tokens)? else { return Err(()) };
    let [Token::Equals, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = parse_expr(tokens)?;
    let stmt = Statement::Assign(*lvalue, expr);
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
    let (Expr::FunctionCall(call), tokens) = parse_expr(tokens)? else { return Err(()) };
    let stmt = Statement::FunctionCall(*call);
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

fn parse_while_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::While, tokens@..] = tokens else { return Err(()) };
    let (cond, tokens) = parse_expr(tokens)?;
    let [Token::Do, tokens@..] = tokens else { return Err(()) };
    let (body, tokens) = parse_body(tokens)?;
    let [Token::End, tokens@..] = tokens else { return Err(()) };

    let stmt = Statement::While(cond, body);
    Ok((stmt, tokens))
}

fn parse_if_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    // if
    let [Token::If, tokens@..] = tokens else { return Err(()) };
    let (cond, tokens) = parse_expr(tokens)?;
    let [Token::Then, tokens@..] = tokens else { return Err(()) };
    let (body, tokens) = parse_body(tokens)?;

    let mut ifblocks = vec![IfBlock(cond, body)];
    let mut tokens = tokens;

    // elseif
    while let [Token::ElseIf, ts@..] = tokens {
        let (cond, ts) = parse_expr(ts)?;
        let [Token::Then, ts@..] = ts else { return Err(()) };
        let (body, ts) = parse_body(ts)?;
        ifblocks.push(IfBlock(cond, body));
        tokens = ts;
    }

    // else
    let mut optelse = None;
    match tokens {
        [Token::End, ts@..] => { tokens = ts; },
        [Token::Else, ts@..] => {
            let (body, ts) = parse_body(ts)?;
            optelse = Some(body);
            let [Token::End, ts@..] = ts else { return Err(()) };
            tokens = ts;
        },
        _ => return Err(()),
    }

    let stmt = Statement::If(ifblocks, optelse);
    Ok((stmt, tokens))
}
