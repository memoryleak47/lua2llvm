use super::*;

pub(in crate::parse::expr) fn get_subexpr_pre(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    Err(())
        .or_else(|_| get_var_subexpr(tokens))
        .or_else(|_| get_lit_subexpr(tokens))
        .or_else(|_| get_ellipsis_subexpr(tokens))
        .or_else(|_| get_function_subexpr(tokens))
        .or_else(|_| get_table_subexpr(tokens))
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

fn get_ellipsis_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Ellipsis, tokens@..] = tokens else { return Err(()) };
    let subexpr = SubExpr::Expr(Expr::Ellipsis);
    Ok((subexpr, tokens))
}

fn get_function_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::Function, Token::LParen, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;

    let mut args = Vec::new();
    let mut variadic = Variadic::No;

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
                [Token::Ellipsis, Token::RParen, ts@..] => {
                    variadic = Variadic::Yes;
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
            let (stat, ts) = parse_statement(tokens)?;
            body.push(stat);
            tokens = ts;
        }
    }

    let expr = Expr::Literal(Literal::Function(args, variadic, body));
    let subexpr = SubExpr::Expr(expr);
    Ok((subexpr, tokens))
}

fn get_table_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    fn get_expr_field(tokens: &[Token]) -> Result<(Field, &[Token]), ()> {
        let (expr, tokens) = parse_expr(tokens)?;
        Ok((Field::Expr(expr), tokens))
    }

    fn get_expr_to_expr_field(tokens: &[Token]) -> Result<(Field, &[Token]), ()> {
        let [Token::LBracket, tokens@..] = tokens else { return Err(()) };
        let (l, tokens) = parse_expr(tokens)?;
        let [Token::RBracket, Token::Equals, tokens@..] = tokens else { return Err(()) };
        let (r, tokens) = parse_expr(tokens)?;
        Ok((Field::ExprToExpr(l, r), tokens))
    }

    fn get_name_to_expr_field(tokens: &[Token]) -> Result<(Field, &[Token]), ()> {
        let [Token::Ident(id), Token::Equals, tokens@..] = tokens else { return Err(()) };
        let (expr, tokens) = parse_expr(tokens)?;
        Ok((Field::NameToExpr(id.clone(), expr), tokens))
    }

    let [Token::LBrace, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;

    let mut fields = Vec::new();

    if let [Token::RBrace, ts@..] = tokens {
        tokens = ts;
    } else {
        loop {
            let (field, ts) = Err(())
                            .or_else(|_| get_name_to_expr_field(tokens))
                            .or_else(|_| get_expr_to_expr_field(tokens))
                            .or_else(|_| get_expr_field(tokens))?;
            fields.push(field);
            match ts {
                [Token::Comma | Token::Semicolon, ts@..] => {
                    tokens = ts;
                },
                [Token::RBrace, ts@..] => {
                    tokens = ts;
                    break;
                },
                _ => return Err(()),
            }
        }
    }

    let expr = Expr::Literal(Literal::Table(fields));
    let subexpr = SubExpr::Expr(expr);
    Ok((subexpr, tokens))
}

fn get_in_paren_subexpr(tokens: &[Token]) -> Result<(SubExpr, &[Token]), ()> {
    let [Token::LParen, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = parse_expr(tokens)?;
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
