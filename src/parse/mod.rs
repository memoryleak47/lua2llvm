use crate::ast::*;
use crate::token::*;

mod statement;
use statement::{parse_statement};
pub use statement::parse;

mod get_subexprs;
use get_subexprs::get_subexprs;

mod assemble_subexprs;
use assemble_subexprs::assemble_subexprs;

// tries to parse the largest-possible prefix expression within `tokens`.
pub(super) fn parse_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), ()> {
    let (subexprs, tokens) = get_subexprs(tokens)?;
    let expr = assemble_subexprs(subexprs)?;

    Ok((expr, tokens))
}

// partial expr, not fully parsed yet.
#[derive(Debug)]
enum SubExpr {
    BinOp(BinOpKind),
    UnOp(UnOpKind),
    CallArgs(Vec<Expr>),
    Expr(Expr), // an already fully parsed Expr
}

#[derive(PartialEq, Eq)]
enum Assoc { Left, Right }

impl SubExpr {
    // returning true means that this SubExpr needs an expression to it's left. like +, ["foo"], .field
    fn left(&self) -> bool {
        matches!(self,
            SubExpr::BinOp(_) | SubExpr::CallArgs(_)
        )
    }

    // analogous to right
    fn right(&self) -> bool {
        matches!(self, SubExpr::BinOp(_) | SubExpr::UnOp(_))
    }

    // the returned value describes the operator priority.
    fn prio(&self) -> u32 {
        use BinOpKind::*;
        match self {
            SubExpr::CallArgs(_) => 1000,
            SubExpr::BinOp(Pow) => 101,
            SubExpr::UnOp(_) => 100,
            SubExpr::BinOp(Mul | Div | Mod) => 99,
            SubExpr::BinOp(Plus | Minus) => 98,
            SubExpr::BinOp(Concat) => 97,
            SubExpr::BinOp(Lt | Le | Gt | Ge | IsEqual | IsNotEqual) => 96,
            SubExpr::BinOp(And) => 95,
            SubExpr::BinOp(Or) => 94,
            _ => 0,
        }
    }

    // if you want 1+2+3 = (1+2)+3, then return Assoc::Left
    // if you want 1+2+3 = 1+(2+3), then return Assoc::Right
    fn assoc(&self) -> Assoc {
        Assoc::Left
    }
}
