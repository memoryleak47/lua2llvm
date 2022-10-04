use super::*;

pub(in crate::parse::expr) fn get_subexpr_post(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    Err(())
        .or_else(|_| get_binop_subexpr(tokens))
        .or_else(|_| get_call_args_subexpr(tokens))
}

fn get_binop_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [tok, tokens@..] = tokens else { return Err(()) };
    let kind = match tok {
        Token::Plus => BinOpKind::Plus,
        Token::Minus => BinOpKind::Minus,
        Token::Mul => BinOpKind::Mul,
        Token::Div => BinOpKind::Div,
        Token::Mod => BinOpKind::Mod,
        Token::And => BinOpKind::And,
        Token::Or => BinOpKind::Or,
        Token::Lt => BinOpKind::Lt,
        Token::Le => BinOpKind::Le,
        Token::Gt => BinOpKind::Gt,
        Token::Ge => BinOpKind::Ge,
        Token::IsEqual => BinOpKind::IsEqual,
        Token::IsNotEqual => BinOpKind::IsNotEqual,
        Token::Concat => BinOpKind::Concat,
        Token::Pow => BinOpKind::Pow,
        _ => return Err(()),
    };
    let subexpr = SubExpr::BinOp(kind);

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
            let (expr, ts) = parse_expr(tokens)?;
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
