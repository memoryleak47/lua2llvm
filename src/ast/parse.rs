use crate::*;

pub fn parse(code: &str) -> Result<Ast, ()> {
    let tokens = tokenize(code);
    assemble(&tokens)
}

fn assemble(mut tokens: &[Token]) -> Result<Ast, ()> {
    let mut ast = Ast { statements: vec![] };

    while !tokens.is_empty() {
        let (st, rest) = assemble_statement(tokens)?;
        tokens = rest;
        ast.statements.push(st);
    }

    Ok(ast)
}

fn assemble_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    Err(())
        .or_else(|_| assemble_assign_statement(tokens))
        .or_else(|_| assemble_function_call_statement(tokens))
        .or_else(|_| assemble_function_def_statement(tokens))
}

fn assemble_function_call_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Ident(fn_name), Token::LParen, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;

    let mut args = Vec::new();

    // zero arg functions
    if let [Token::RParen, ts@..] = tokens {
        tokens = ts;
    } else { // >= 1 arg functions
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
                _ => return Err(()),
            }
        }
    }
    let stmt = Statement::FunctionCall { fn_name: fn_name.clone(), args };
    Ok((stmt, tokens))
}

fn assemble_function_def_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Function, Token::Ident(fn_name), Token::LParen, tokens@..] = tokens else { return Err(()) };
    let mut tokens = tokens;

    let mut args = Vec::new();

    loop {
        match tokens {
            [Token::Ident(ident), Token::Comma, ts@..] => {
                args.push(ident.clone());
                tokens = ts;
            },
            [Token::RParen, ts@..] => {
                tokens = ts;
                break;
            },
            _ => return Err(()),
        }
    }

    let mut statements = Vec::new();
    loop {
        if let [Token::End, ts@..] = tokens {
            tokens = ts;
            break;
        } else {
            let (stat, ts) = assemble_statement(tokens)?;
            statements.push(stat);
            tokens = ts;
        }
    }

    let stmt = Statement::FunctionDef {
        fn_name: fn_name.clone(),
        args,
        body: statements,
    };

    Ok((stmt, tokens))
}

fn assemble_assign_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ()> {
    let [Token::Ident(var), tokens@..] = tokens else { return Err(()) };
    let [Token::Equals, tokens@..] = tokens else { return Err(()) };
    let (expr, tokens) = assemble_expr(tokens)?;
    let stmt = Statement::Assign { var: var.to_string(), expr };
    Ok((stmt, tokens))
}

fn assemble_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    let (mut expr, mut rest) = assemble_atomic_expr(tokens)?;
    while let [Token::Plus, next_rest@..]= rest {
        let (next_expr, next_rest) = assemble_atomic_expr(next_rest)?;
        expr = Expr::Plus(Box::new(expr), Box::new(next_expr));
        rest = next_rest;
    }

    Ok((expr, rest))
}

fn assemble_atomic_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    Err(())
        .or_else(|_| assemble_var_expr(tokens))
        .or_else(|_| assemble_num_expr(tokens))
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
