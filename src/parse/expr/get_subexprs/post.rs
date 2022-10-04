use super::*;

pub(in crate::parse::expr) fn get_subexpr_post(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    Err(())
        .or_else(|_| get_binop_subexpr(tokens))
        .or_else(|_| get_call_args_subexpr(tokens))
        .or_else(|_| get_index_subexpr(tokens))
        .or_else(|_| get_dot_subexpr(tokens))
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

fn get_call_args_subexpr(mut tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    // parse optional :foo
    let mut colon_arg = None;
    if let [Token::Colon, Token::Ident(var), ts@..] = tokens {
        tokens = ts;
        colon_arg = Some(var.clone());
    }

    // parse (<args>)
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

    let subexpr = SubExpr::CallArgs(colon_arg, args);
    Ok((subexpr, tokens))
}

fn get_index_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::LBracket, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = parse_expr(tokens)?;
    let [Token::RBracket, tokens@..] = tokens else { return Err(()) };

    let subexpr = SubExpr::Index(expr);
    Ok((subexpr, tokens))
}

fn get_dot_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Dot, Token::Ident(field), tokens@..] = tokens else { return Err(()) };

    let subexpr = SubExpr::Dot(field.clone());
    Ok((subexpr, tokens))
}
