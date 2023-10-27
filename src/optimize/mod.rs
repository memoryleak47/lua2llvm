use crate::ir::*;
use crate::infer::*;
use std::hash::Hash;

pub mod util;
use util::*;

#[derive(PartialEq, Eq)]
enum Changed { Yes, No }
type Optimization = fn(&mut IR, &mut Infer) -> Changed;

static OPTIMIZATIONS: &'static [Optimization] = &[rm_unused_node, resolve_const_compute, rm_unread_store, rm_unused_fns];

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
        ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid).unwrap()[sid] = Statement::Compute(node, new_expr);
        return Changed::Yes;
    }

    Changed::No
}

// Three reasons why a store might not be optimized out:
// - someone indexes into the table
// - someone uses next on the table
// - someone accesses the length of the table
fn rm_unread_store(ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut unread_stores: Set<Stmt> = Set::new();
    for stmt in stmts(ir) {
        if let Statement::Store(_, _, _) = deref_stmt(stmt, ir) {
            unread_stores.insert(stmt);
        }
    }

    for stmt in stmts(ir) {
        let Statement::Compute(_, Expr::Index(t, k)) = deref_stmt(stmt, ir) else { continue; };
        let loc: &LocalState = &inf.local_state[&stmt];
        if !loc.executed { continue; }
        let t = &loc.nodes[&t];
        let k = &loc.nodes[&k];
        let entry = loc.class_states.get_entry(t, k);
        for x in &entry.sources {
            unread_stores.remove(x);
        }
    }

    for stmt in stmts(ir) {
        let Statement::Compute(_, Expr::Intrinsic(Intrinsic::Next(t, _)) | Expr::Len(t)) = deref_stmt(stmt, ir) else { continue; };
        let loc: &LocalState = &inf.local_state[&stmt];
        if !loc.executed { continue; }
        let t = &loc.nodes[&t];
        for cl in &t.classes {
            for (_, entry) in &loc.class_states.0[cl].0 {
                for x in &entry.sources {
                    unread_stores.remove(x);
                }
            }
        }
    }

    if unread_stores.is_empty() {
        Changed::No
    } else {
        rm_stmts(unread_stores.iter().copied().collect(), ir, inf);
        Changed::Yes
    }
}

fn rm_unused_fns(ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut open: Set<FnId> = Set::new();
    open.insert(ir.main_fn);

    loop {
        let len1 = open.len();
        for &fid in open.clone().iter() {
            for stmt in stmts_in_fid(fid, ir) {
                if let Statement::Compute(_, Expr::LitFunction(fid_)) = deref_stmt(stmt, ir) {
                    open.insert(fid_);
                }
            }
        }

        let len2 = open.len();
        if len1 == len2 { break; }
    }

    let unused: Vec<FnId> = ir.fns.keys().copied().filter(|fid| !open.contains(fid)).collect();

    if !unused.is_empty() {
        rm_fns(unused, ir, inf);
        Changed::Yes
    } else {
        Changed::No
    }
}
