use crate::hir::*;
use crate::ir::{IR, self};

pub fn lower_hir(hir: &HIR) -> IR {
    IR {
        fns: hir.fns.iter().map(|(x, y)| (*x, lower_function(y))).collect(),
        main_fn: hir.main_fn,
        table_layouts: Default::default(), // TODO this should be default-set to TableLayout::HashMap everywhere.
    }
}

fn lower_function(f: &Function) -> ir::Function {
    ir::Function {
        blocks: f.blocks.iter().map(|(x, y)| (*x, lower_block(y))).collect(),
        start_block: f.start_block,
    }
}

fn lower_block(b: &Block) -> ir::Block {
    b.iter().map(|x| lower_statement(x)).collect()
}

fn lower_statement(st: &Statement) -> ir::Statement {
    match st {
        Statement::Compute(n, expr) => ir::Statement::Compute(*n, lower_expr(expr)),
        Statement::Store(a, b, c) => ir::Statement::Store(*a, *b, *c),
        Statement::If(a, b, c) => ir::Statement::If(*a, *b, *c),
        Statement::FnCall(a, b) => ir::Statement::FnCall(*a, *b),
        Statement::Print(a) => ir::Statement::Print(*a),
        Statement::Throw(s) => ir::Statement::Throw(s.to_string()),
        Statement::Return => ir::Statement::Return,
    }
}

fn lower_expr(expr: &Expr) -> ir::Expr {
    match expr {
        Expr::Index(a, b) => ir::Expr::Index(*a, *b),
        Expr::Arg => ir::Expr::Arg,
        Expr::NewTable => ir::Expr::NewTable,
        Expr::Function(f) => ir::Expr::Function(*f),
        Expr::BinOp(kind, l, r) => ir::Expr::BinOp(lower_binop_kind(kind), *l, *r),
        Expr::Len(a) => ir::Expr::Len(*a),
        Expr::Next(a, b) => ir::Expr::Next(*a, *b),
        Expr::Type(a) => ir::Expr::Type(*a),
        Expr::Num(a) => ir::Expr::Num(*a),
        Expr::Bool(a) => ir::Expr::Bool(*a),
        Expr::Nil => ir::Expr::Nil,
        Expr::Str(a) => ir::Expr::Str(a.clone()),
    }
        
}

fn lower_binop_kind(kind: &BinOpKind) -> ir::BinOpKind {
    match kind {
        BinOpKind::Plus => ir::BinOpKind::Plus,
        BinOpKind::Minus => ir::BinOpKind::Minus,
        BinOpKind::Mul => ir::BinOpKind::Mul,
        BinOpKind::Div => ir::BinOpKind::Div,
        BinOpKind::Mod => ir::BinOpKind::Mod,
        BinOpKind::Lt => ir::BinOpKind::Lt,
        BinOpKind::Le => ir::BinOpKind::Le,
        BinOpKind::Gt => ir::BinOpKind::Gt,
        BinOpKind::Ge => ir::BinOpKind::Ge,
        BinOpKind::IsEqual => ir::BinOpKind::IsEqual,
        BinOpKind::IsNotEqual => ir::BinOpKind::IsNotEqual,
        BinOpKind::Concat => ir::BinOpKind::Concat,
        BinOpKind::Pow => ir::BinOpKind::Pow,
    }
}
