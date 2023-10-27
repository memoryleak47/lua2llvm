use crate::ir::*;
use crate::infer::*;

pub fn stmts(ir: &IR) -> Vec<Stmt> {
    let mut out = Vec::new();

    for fid in 0..ir.fns.len() {
        out.extend(stmts_in_fid(fid, ir));
    }

    out
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

pub fn rm_stmt((fid, bid, sid): Stmt, ir: &mut IR, inf: &mut Infer) {
    *inf = inf.erase_stmt((fid, bid, sid), ir);
    ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid).unwrap().remove(sid);
    // how do I remove every relevant thing for a stmt?
    // - written nodes
    // - classes allocated in this stmt
    // - Store-entry sources

    *inf = inf.map_stmt(&|(fid_, bid_, sid_)| {
        // If this assert fails, the old infer state still made use of the removed stmt somewhere.
        assert!((fid_, bid_, sid_) != (fid, bid, sid));

        if (fid_, bid_) == (fid, bid) && sid_ > sid {
            (fid_, bid_, sid_ - 1)
        } else {
            (fid_, bid_, sid_)
        }
    });
}

pub fn rm_stmts(mut stmts: Vec<Stmt>, ir: &mut IR, inf: &mut Infer) {
    // Later stmts should be removed earlier so that the indices don't get messed up.
    stmts.sort_by_key(|(_fid, _bid, sid)| *sid);
    stmts.reverse();
    for stmt in stmts {
        rm_stmt(stmt, ir, inf);
    }
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
            Statement::Command(Command::Print(n)) => *n == node,
            Statement::Command(Command::Throw(_)) => false,
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
            Expr::LitFunction(_) => false,
            Expr::BinOp(_, l, r) => [l, r].contains(&&node),
            Expr::Len(n) => *n == node,
            Expr::Intrinsic(Intrinsic::Next(t, i)) => [t, i].contains(&&node),
            Expr::Intrinsic(Intrinsic::Type(t)) => *t == node,
            Expr::Num(_) => false,
            Expr::Bool(_) => false,
            Expr::Nil => false,
            Expr::Str(_) => false,
        }
    }
}

// for Compute Statements, this returns the node. otherwise, None.
pub fn get_node_from_stmt(stmt: Stmt, ir: &IR) -> Option<Node> {
    match deref_stmt(stmt, ir) {
        Statement::Compute(n, _) => Some(n),
        _ => None,
    }
}
