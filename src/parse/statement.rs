use crate::parse::{expr::parse_expr, parse_body};
use crate::ast::*;
use crate::token::Token;

// parse_statement

pub(super) fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let (stmt, mut tokens) = Err(())
        .or_else(|_| parse_assign_statement(tokens))
        .or_else(|_| parse_local_statement(tokens))
        .or_else(|_| parse_function_call_statement(tokens))
        .or_else(|_| parse_return_statement(tokens))
        .or_else(|_| parse_break_statement(tokens))
        .or_else(|_| parse_while_statement(tokens))
        .or_else(|_| parse_if_statement(tokens))?;

    // optional semicolon.
    if let [Token::Semicolon, ts@..] = tokens {
        tokens = ts;
    }

    Ok((stmt, tokens))
}

// parses comma separated expression lists for this usecase:
// [local] <expr-list> = <expr-list>
fn parse_expr_list(tokens: &[Token]) -> Result<(Vec<Expr>, &[Token]), ()> {
    let mut exprs = Vec::new();

    let (expr, mut tokens) = parse_expr(tokens)?;
    exprs.push(expr);

    while let [Token::Comma, ts@..] = tokens {
        let (expr, ts) = parse_expr(ts)?;
        exprs.push(expr);
        tokens = ts;
    }

    Ok((exprs, tokens))
}

fn parse_lvalue_list(tokens: &[Token]) -> Result<(Vec<LValue>, &[Token]), ()> {
    let (exprs, tokens) = parse_expr_list(tokens)?;
    let mut lvalues = Vec::new();
    for val in exprs {
        let Expr::LValue(lvalue) = val else { return Err(()) };
        lvalues.push(*lvalue);
    }
    Ok((lvalues, tokens))
}

fn parse_ident_list(tokens: &[Token]) -> Result<(Vec<String>, &[Token]), ()> {
    let (lvalues, tokens) = parse_lvalue_list(tokens)?;
    let mut idents = Vec::new();
    for lvalue in lvalues {
        let LValue::Var(ident) = lvalue else { return Err(()) };
        idents.push(ident);
    }
    Ok((idents, tokens))
}

fn parse_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let (lhs, tokens) = parse_lvalue_list(tokens)?;
    let [Token::Equals, tokens@..] = tokens else { return Err(()) };
    let (rhs, tokens) = parse_expr_list(tokens)?;
    let stmt = Statement::Assign(lhs, rhs);
    Ok((stmt, tokens))
}

fn parse_local_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Local, tokens@..] = tokens else { return Err(()) };
    let (idents, tokens) = parse_ident_list(tokens)?;
    let [Token::Equals, tokens@..] = tokens else {
        let stmt = Statement::Local(idents, Vec::new());
        return Ok((stmt, tokens))
    };
    let (rhs, tokens) = parse_expr_list(tokens)?;
    let stmt = Statement::Local(idents, rhs);
    Ok((stmt, tokens))
}

fn parse_function_call_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let (Expr::FunctionCall(call), tokens) = parse_expr(tokens)? else { return Err(()) };
    let stmt = Statement::FunctionCall(*call);
    Ok((stmt, tokens))
}

fn parse_return_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Return, tokens@..] = tokens else { return Err(()) };
    let (exprs, tokens) = parse_expr_list(tokens)
                        .unwrap_or_else(|()| (Vec::new(), tokens));
    let stmt = Statement::Return(exprs);
    Ok((stmt, tokens))
}

fn parse_break_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Break, tokens@..] = tokens else { return Err(()) };
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
