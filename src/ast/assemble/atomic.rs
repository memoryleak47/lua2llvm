use super::*;

pub(super) fn assemble_atomic_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    Err(())
        .or_else(|_| assemble_var_expr(tokens))
        .or_else(|_| assemble_num_expr(tokens))
        .or_else(|_| assemble_function_expr(tokens))
}

fn assemble_var_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    let [Token::Ident(var), rest@..] = &tokens else { return Err(()) };
    let expr = Expr::Var(var.to_string());
    Ok((expr, rest))
}

fn assemble_num_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    let [Token::LiteralNum(x), rest@..] = tokens else { return Err(()) };
    let expr = Expr::LiteralNum(*x);
    Ok((expr, rest))
}

fn assemble_function_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
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
    Ok((expr, tokens))
}
