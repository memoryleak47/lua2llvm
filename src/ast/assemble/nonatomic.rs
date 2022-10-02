use super::*;

pub(super) fn assemble_nonatomic_expr<'t>(tokens: &'t [Token], expr: &Expr) -> Result<(Expr, &'t [Token]), ()> {
    Err(())
        .or_else(|_| assemble_op_expr(tokens, expr))
        .or_else(|_| assemble_call_expr(tokens, expr))
}

fn assemble_op_expr<'t>(tokens: &'t [Token], expr: &Expr) -> Result<(Expr, &'t [Token]), ()> {
    let [Token::Plus, tokens@..] = tokens else { return Err(()) };
    let (next, tokens) = assemble_expr(tokens)?;
    let expr = Expr::Plus(Box::new(expr.clone()), Box::new(next));
    Ok((expr, tokens))
}

fn assemble_call_expr<'t>(tokens: &'t [Token], func: &Expr) -> Result<(Expr, &'t [Token]), ()> {
    let [Token::LParen, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;
    let mut args = Vec::new();

    if let [Token::RParen, ts@..] = tokens {
        tokens = ts;
    } else {
        loop {
            let (arg, ts) = assemble_expr(tokens)?;
            args.push(arg);

            match ts {
                [Token::RParen, ts@..] => {
                    tokens = ts;
                    break;
                },
                [Token::Comma, ts@..] => {
                    tokens = ts;
                    continue;
                },
                _ => panic!()
            }
        }
    }

    let expr = Expr::FunctionCall { func: Box::new(func.clone()), args };
    Ok((expr, tokens))
}
