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
        .or_else(|_| get_lit_subexpr(tokens))
        .or_else(|_| get_function_subexpr(tokens))
        .or_else(|_| get_in_paren_subexpr(tokens))
        .or_else(|_| get_unop_subexpr(tokens))
}

fn get_var_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Ident(ident), tokens@..] = tokens else { return Err(()) };
    let lvalue = LValue::Var(ident.clone());
    let expr = Expr::LValue(Box::new(lvalue));
    let subexpr = SubExpr::Expr(expr);

    Ok((subexpr, tokens))
}

// Literals in subexpressions, this does not include functions though.
fn get_lit_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let (lit, tokens) = match tokens {
        [Token::True, ts@..] => (Literal::Bool(true), ts),
        [Token::False, ts@..] => (Literal::Bool(false), ts),
        [Token::LiteralNum(x), ts@..] => (Literal::Num(*x), ts),
        [Token::LiteralStr(x), ts@..] => (Literal::Str(x.clone()), ts),
        [Token::Nil, ts@..] => (Literal::Nil, ts),
        _ => return Err(()),
    };
    let subexpr = SubExpr::Expr(Expr::Literal(lit));
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

    let expr = Expr::Literal(Literal::Function(args, body));
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

fn get_unop_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let (kind, tokens) = match tokens {
        [Token::Len, ts@..] => (UnOpKind::Len, ts),
        [Token::Minus, ts@..] => (UnOpKind::Neg, ts),
        [Token::Not, ts@..] => (UnOpKind::Not, ts),
        _ => return Err(()),
    };
    let subexpr = SubExpr::UnOp(kind);
    Ok((subexpr, tokens))
}

// post

fn get_subexpr_post(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
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
