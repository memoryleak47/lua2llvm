use crate::ir::*;
use crate::optimize::*;

pub fn stmts(ir: &IR) -> Vec<Stmt> {
    let mut out = Vec::new();

    for &fid in ir.fns.keys() {
        out.extend(stmts_in_fid(fid, ir));
    }

    out
}

pub fn stmt_executed((fid, bid, sid): Stmt, inf: &Infer) -> bool {
    specs_of_stmt((fid, bid, sid), inf).iter().any(|(_, rt_stack)|
        inf.local_state[rt_stack].executed
    )
}

pub fn specs_of_stmt((fid, bid, sid): Stmt, inf: &Infer) -> Vec<(FnSpec, RtStack)> {
    let mut out = Vec::new();

    for spec in specs_of_fn(fid, inf) {
        let mut rt_stack = spec.rt_stack.clone();
        rt_stack.push((fid, bid, sid));
        out.push((spec, rt_stack));
    }

    out
}

pub fn specs_of_fn(fid: FnId, inf: &Infer) -> Vec<FnSpec> {
    inf.fn_state.keys().filter(|spec| spec.fid == fid).cloned().collect()
}

pub fn stmts_in_fid(fid: FnId, ir: &IR) -> Vec<Stmt> {
    let mut out = Vec::new();
    for (&bid, statements) in ir.fns[&fid].blocks.iter() {
        for sid in 0..statements.len() {
            out.push((fid, bid, sid));
        }
    }

    out
}

pub fn deref_stmt((fid, bid, sid): Stmt, ir: &IR) -> Statement {
    ir.fns[&fid].blocks[&bid][sid].clone()
}

impl Statement {
    pub fn uses_node(&self, node: Node) -> bool {
        match self {
            // computing the node doesn't count as "using" it.
            Statement::Compute(_, expr) => expr.uses_node(node),
            Statement::Store(t, i, v) => [t, i, v].contains(&&node),
            Statement::If(c, _, _) => *c == node,
            Statement::FnCall(f, arg) => [f, arg].contains(&&node),
            Statement::Print(n) => *n == node,
            Statement::Throw(_) => false,
            Statement::Return => false,
        }
    }
}

impl Expr {
    pub fn uses_node(&self, node: Node) -> bool {
        match self {
            Expr::Index(table, idx) => [table, idx].contains(&&node),
            Expr::Arg => false,
            Expr::NewTable => false,
            Expr::Function(_) => false,
            Expr::BinOp(_, l, r) => [l, r].contains(&&node),
            Expr::Len(n) => *n == node,
            Expr::Next(t, i) => [t, i].contains(&&node),
            Expr::Type(t) => *t == node,
            Expr::Num(_) => false,
            Expr::Bool(_) => false,
            Expr::Nil => false,
            Expr::Str(_) => false,
        }
    }
}
