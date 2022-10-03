use super::*;

mod get_subexprs;
use get_subexprs::get_subexprs;

mod assemble_subexprs;
use assemble_subexprs::assemble_subexprs;

// tries to assemble the largest-possible prefix expression within `tokens`.
pub(super) fn assemble_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    let (subexprs, tokens) = get_subexprs(tokens)?;
    let expr = assemble_subexprs(subexprs)?;

    Ok((expr, tokens))
}

// partial expr, not fully parsed yet.
#[derive(Debug)]
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
