use crate::ir::{IR, FnId, Statement, Node, Expr, Intrinsic, Command};
use crate::infer::{Infer, Stmt, Value, Lattice, Set};
use std::hash::Hash;

#[derive(PartialEq, Eq)]
enum Changed { Yes, No }
type Optimization = fn(&mut IR, &mut Infer) -> Changed;

static OPTIMIZATIONS: &'static [Optimization] = &[rm_unused_node, resolve_const_compute];

pub fn optimize(ir: &mut IR, inf: &mut Infer) {
    loop {
        let mut changed = Changed::No;
        for o in OPTIMIZATIONS {
            if o(ir, inf) == Changed::Yes {
                changed = Changed::Yes;
            }
        }

        if changed == Changed::No { break; }
    }
}

fn stmts(ir: &IR) -> Vec<Stmt> {
    let mut out = Vec::new();
    for fid in 0..ir.fns.len() {
        out.extend(stmts_in_fid(fid, ir));
    }

    out
}

fn stmts_in_fid(fid: FnId, ir: &IR) -> Vec<Stmt> {
    let mut out = Vec::new();
    for (bid, statements) in ir.fns[fid].blocks.iter().enumerate() {
        for sid in 0..statements.len() {
            out.push((fid, bid, sid));
        }
    }

    out
}

fn rm_stmt((fid, bid, sid): Stmt, ir: &mut IR, inf: &mut Infer) {
    ir.fns[fid].blocks[bid].remove(sid);
    inf.local_state.remove(&(fid, bid, sid));

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

fn deref_stmt((fid, bid, sid): Stmt, ir: &IR) -> Statement {
    ir.fns[fid].blocks[bid][sid].clone()
}

impl Statement {
    fn uses_node(&self, node: Node) -> bool {
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
    fn uses_node(&self, node: Node) -> bool {
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

fn rm_unused_node(ir: &mut IR, inf: &mut Infer) -> Changed {
    for (fid, bid, sid) in stmts(ir) {
        let Statement::Compute(node, _) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if stmts_in_fid(fid, ir).iter().all(|stmt2| !deref_stmt(*stmt2, ir).uses_node(node)) {
            rm_stmt((fid, bid, sid), ir, inf);
            return Changed::Yes;
        }
    }

    return Changed::No;
}

fn resolve_const_compute(ir: &mut IR, inf: &mut Infer) -> Changed {
    for (fid, bid, sid) in stmts(ir) {
        if !inf.local_state[&(fid, bid, sid)].executed { continue; };

        let Statement::Compute(node, expr) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if matches!(expr, Expr::LitFunction(_) | Expr::Num(_) | Expr::Bool(_) | Expr::Nil | Expr::Str(_)) { continue; };

        let val = inf.local_state[&(fid, bid, sid+1)].nodes[&node].clone();
        if !val.is_concrete() { continue; }
        if !val.classes.is_empty() { continue; }
        if val == Value::bot() { continue; }

        fn extract_from_set<T: Clone + Hash + Eq>(set: &Set<T>) -> Option<T> {
            assert!(set.len() <= 1);
            set.iter().next().cloned()
        }

        fn extract_from_lattice<T: Clone + Hash + Eq>(lattice: &Lattice<T>) -> Option<T> {
            let Lattice::Set(set) = lattice else { panic!(); };
            extract_from_set(set)
        }

        // exactly one of these exprs should be Some.
        let l: [Option<Expr>; 5] = [
            extract_from_lattice(&val.strings).map(Expr::Str),
            extract_from_set(&val.fns).map(Expr::LitFunction),
            extract_from_lattice(&val.nums).map(|x| x.into()).map(Expr::Num),
            extract_from_set(&val.nils).map(|_| Expr::Nil),
            extract_from_set(&val.bools).map(Expr::Bool),
        ];
        let new_expr: Expr = l.into_iter().map(|x| x.into_iter()).flatten().next().unwrap().clone();
        ir.fns[fid].blocks[bid][sid] = Statement::Compute(node, new_expr);
        return Changed::Yes;
    }

    Changed::No
}
