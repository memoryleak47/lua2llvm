use super::*;

// get_subexprs

pub(super) fn get_subexprs(mut tokens: &[Token]) -> Result<(Vec<SubExpr>, &[Token]), ()> {
    let mut subexprs: Vec<SubExpr> = Vec::new();

    while let Ok((next, ts)) = 
            match subexprs.last() {
                // we've already gotten an Expr, we are "post" an expression
                // example: 2+4
                Some(s) if !s.right() => get_subexpr_post(tokens),

                // we need an expression, we are "pre" an expression.
                // example: 2+
                _ => get_subexpr_pre(tokens),
            } {
        tokens = ts;
        subexprs.push(next);
    }

    Ok((subexprs, tokens))
}

// pre

fn get_subexpr_pre(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    Err(())
        .or_else(|_| get_var_subexpr(tokens))
        .or_else(|_| get_num_subexpr(tokens))
        .or_else(|_| get_function_subexpr(tokens))
        .or_else(|_| get_in_paren_subexpr(tokens))
}

fn get_var_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Ident(ident), tokens@..] = tokens else { return Err(()) };
    let subexpr = SubExpr::Expr(Expr::Var(ident.clone()));

    Ok((subexpr, tokens))
}

fn get_num_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::LiteralNum(x), tokens@..] = tokens else { return Err(()) };
    let subexpr = SubExpr::Expr(Expr::LiteralNum(*x));

    Ok((subexpr, tokens))
}

fn get_function_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Function, Token::LParen, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;

    let mut args = Vec::new();

    if let [Token::RParen, ts@..] = tokens {
        tokens = ts;
    } else {
        loop {
            match tokens {
                [Token::Ident(ident), Token::Comma, ts@..] => {
                    args.push(ident.clone());
                    tokens = ts;
                },
                [Token::Ident(ident), Token::RParen, ts@..] => {
                    args.push(ident.clone());
                    tokens = ts;
                    break;
                },
                _ => return Err(()),
            }
        }
    }

    let mut body = Vec::new();
    loop {
        if let [Token::End, ts@..] = tokens {
            tokens = ts;
            break;
        } else {
            let (stat, ts) = assemble_statement(tokens)?;
            body.push(stat);
            tokens = ts;
        }
    }

    let expr = Expr::Function { args, body, };
    let subexpr = SubExpr::Expr(expr);
    Ok((subexpr, tokens))
}


fn get_in_paren_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::LParen, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = assemble_expr(tokens)?;
    let [Token::RParen, tokens@..] = tokens else { return Err(()) };

    let subexpr = SubExpr::Expr(expr);
    Ok((subexpr, tokens))
}

// post

fn get_subexpr_post(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    Err(())
        .or_else(|_| get_plus_subexpr(tokens))
        .or_else(|_| get_call_args_subexpr(tokens))
}

fn get_plus_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Plus, tokens@..] = tokens else { return Err(()) };
    let subexpr = SubExpr::Plus;

    Ok((subexpr, tokens))
}

fn get_call_args_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::LParen, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;
    let mut args = Vec::new();

    if let [Token::RParen, ts@..] = tokens {
        tokens = ts;
    } else {
        loop {
            let (expr, ts) = assemble_expr(tokens)?;
            args.push(expr);
            match ts {
                [Token::RParen, ts@..] => {
                    tokens = ts;
                    break;
                },
                [Token::Comma, ts@..] => {
                    tokens = ts;
                },
                _ => panic!("unexpected token!"),
            }
        }
    }

    let subexpr = SubExpr::CallArgs(args);
    Ok((subexpr, tokens))
}
