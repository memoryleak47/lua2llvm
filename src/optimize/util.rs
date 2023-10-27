use crate::ir::*;
use crate::infer::*;
use crate::optimize::*;

pub fn stmts(ir: &IR) -> Vec<Stmt> {
    let mut out = Vec::new();

    for &fid in ir.fns.keys() {
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

pub fn rm_stmts(mut stmts: Vec<Stmt>, ir: &mut IR, inf: &mut Infer) -> Changed {
    // Later stmts should be removed earlier so that the indices don't get messed up.
    stmts.sort_by_key(|(_fid, _bid, sid)| *sid);
    stmts.reverse();
    let mut changed = Changed::No;
    for stmt in stmts {
        changed = Changed::Yes;
        rm_stmt(stmt, ir, inf);
    }
    changed
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

// Should only be called for functions that never get called, and in particular whose Expr::LitFunction is never constructed.
pub fn rm_fn(fid: FnId, ir: &mut IR, inf: &mut Infer) {
    inf.local_state.retain(|(fid_, _, _), _| *fid_ != fid);
    inf.fn_state.retain(|fid_, _| *fid_ != fid);
    ir.fns.remove(&fid);

    inf.map_stmt(&|(fid_, _, _)| {
        assert!(fid_ != fid);
        (0, 0, 0)
    });
}

pub fn rm_fns(fids: Vec<FnId>, ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut changed = Changed::No;
    for fid in fids {
        rm_fn(fid, ir, inf);
        changed = Changed::Yes;
    }
    changed
}

// only allowed to be called, if this these blocks where never executed.
pub fn rm_block(fid: FnId, bid: BlockId, ir: &mut IR, inf: &mut Infer) {
    inf.local_state.retain(|&(fid_, bid_, _), _| (fid_, bid_) != (fid, bid));

    ir.fns.get_mut(&fid).unwrap().blocks.remove(&bid);
}

pub fn rm_blocks(blocks: Vec<(FnId, BlockId)>, ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut changed = Changed::No;
    for (fid, bid) in blocks {
        changed = Changed::Yes;
        rm_block(fid, bid, ir, inf);
    }
    changed
}
