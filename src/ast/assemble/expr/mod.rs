use super::*;

mod assemble;
use assemble::assemble_subexprs;

// partial expr, not fully parsed yet.
enum SubExpr {
    Plus,
    CallArgs(Vec<Expr>),
    Expr(Expr), // an already fully parsed Expr
}

#[derive(PartialEq, Eq)]
enum Assoc { Left, Right }

impl SubExpr {
    // returning true means that this SubExpr needs an expression to it's left. like +, ["foo"], .field
    fn left(&self) -> bool {
        match self {
            SubExpr::Plus => true,
            SubExpr::CallArgs(_) => true,
            _ => false,
        }
    }

    // analogous to right
    fn right(&self) -> bool {
        match self {
            SubExpr::Plus => true,
            _ => false,
        }
    }

    // the returned value describes the operator priority.
    fn prio(&self) -> u32 {
        match self {
            SubExpr::CallArgs(_) => 1000,
            SubExpr::Plus => 100,
            _ => 0,
        }
    }

    // if you want 1+2+3 = (1+2)+3, then return Assoc::Left
    // if you want 1+2+3 = 1+(2+3), then return Assoc::Right
    fn assoc(&self) -> Assoc {
        Assoc::Left
    }
}

// tries to assemble the largest-possible prefix expression within `tokens`.
pub(super) fn assemble_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    let (subexprs, tokens) = get_subexprs(tokens)?;
    let expr = assemble_subexprs(subexprs)?;

    Ok((expr, tokens))
}

// get_subexprs

fn get_subexprs(mut tokens: &[Token]) -> Result<(Vec<SubExpr>, &[Token]), ()> {
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
