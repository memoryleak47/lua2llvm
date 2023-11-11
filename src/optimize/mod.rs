use crate::ir::*;
use crate::infer::*;
use std::hash::Hash;

pub mod util;
use util::*;

mod changes;
use changes::*;

type Optimization = fn(&IR, &Infer, &mut Changes);

static OPTIMIZATIONS: &'static [Optimization] = &[rm_unused_node, rm_unused_fns, rm_unused_blocks, resolve_const_compute, rm_unread_store, resolve_const_ifs, hollow_uncalled_fns, merge_blocks];

pub fn optimize(ir: &mut IR) {
    loop {
        let inf = infer(ir);

        let mut changes = Changes::default();
        for o in OPTIMIZATIONS {
            o(ir, &inf, &mut changes);
        }
        if changes.is_empty() { break; }

        changes.apply(ir);
    }
}

// infer-independent opts:

fn rm_unused_node(ir: &IR, #[allow(unused)] inf: &Infer, changes: &mut Changes) {
    for (fid, bid, sid) in stmts(ir) {
        let Statement::Compute(node, _) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if stmts_in_fid(fid, ir).iter().all(|stmt2| !deref_stmt(*stmt2, ir).uses_node(node)) {
            changes.rm_stmts.insert((fid, bid, sid));
        }
    }
}

fn rm_unused_fns(ir: &IR, #[allow(unused)] inf: &Infer, changes: &mut Changes) {
    let mut open: Set<FnId> = Set::new();
    open.insert(ir.main_fn);

    loop {
        let len1 = open.len();
        for &fid in open.clone().iter() {
            for stmt in stmts_in_fid(fid, ir) {
                if let Statement::Compute(_, Expr::Function(fid_)) = deref_stmt(stmt, ir) {
                    open.insert(fid_);
                }
            }
        }

        let len2 = open.len();
        if len1 == len2 { break; }
    }

    changes.rm_fns.extend(ir.fns.keys().copied().filter(|fid| !open.contains(fid)));
}

// TODO we could use the infer to find even more merge opportunities. Some branches are never taken, see resolve_const_ifs.
fn merge_blocks(ir: &IR, #[allow(unused)] inf: &Infer, changes: &mut Changes) {
    fn out_blocks(fid: FnId, bid: BlockId, ir: &IR) -> Vec<BlockId> {
        if let Some(Statement::If(_, then_bid, else_bid)) = ir.fns[&fid].blocks[&bid].last() {
            // filter duplicates!
            if then_bid == else_bid { vec![*then_bid] }
            else { vec![*then_bid, *else_bid] }
        } else { vec![] }
    }

    for &fid in ir.fns.keys() {
        let mut ptrs: Vec<(BlockId, Vec<BlockId>)> = Vec::new();
        for &bid1 in ir.fns[&fid].blocks.keys() {
            let outs = out_blocks(fid, bid1, ir);
            ptrs.push((bid1, outs));
        }

        for &bid2 in ir.fns[&fid].blocks.keys() {
            if ir.fns[&fid].start_block == bid2 { continue; }

            // subset of `ptrs` which points to `bid2`.
            let ptrs_to_bid2: Vec<_> = ptrs.iter().filter(|&(_, targets)| targets.contains(&bid2)).cloned().collect();

            // we require exactly *one* block to point to `bid2`.
            let &[(bid1, ref targets)] = &ptrs_to_bid2[..] else { continue; };

            // we require it to *only* point to `bid2`.
            if targets == &vec![bid2] {
                changes.merge_blocks.insert((fid, bid1, bid2));
            }
        }
    }
}

// infer-dependent opts:

fn resolve_const_compute(ir: &IR, inf: &Infer, changes: &mut Changes) {
    for (fid, bid, sid) in stmts(ir) {
        let Statement::Compute(node, expr) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if matches!(expr, Expr::Function(_) | Expr::Num(_) | Expr::Bool(_) | Expr::Nil | Expr::Str(_)) { continue; };

        let val = specs_of_stmt((fid, bid, sid), inf).iter().map(|(_, rt_stack)| {
            let mut follow_up_rt_stack: RtStack = rt_stack.clone();
            follow_up_rt_stack.last_mut().unwrap().2 += 1;
            let loc = &inf.local_state[&follow_up_rt_stack];
            if loc.executed {
                loc.nodes[&node].clone()
            } else {
                Value::bot()
            }
        }).fold(Value::bot(), |x, y| Value::merge(&x, &y));

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
            extract_from_set(&val.fns).map(Expr::Function),
            extract_from_lattice(&val.nums).map(|x| x.into()).map(Expr::Num),
            extract_from_set(&val.nils).map(|_| Expr::Nil),
            extract_from_set(&val.bools).map(Expr::Bool),
        ];
        let new_expr: Expr = l.into_iter().map(|x| x.into_iter()).flatten().next().unwrap().clone();
        changes.change_stmts.insert(((fid, bid, sid), Statement::Compute(node, new_expr)));
    }

}

// Three reasons why a store might not be optimized out:
// - someone indexes into the table
// - someone uses next on the table
// - someone accesses the length of the table
fn rm_unread_store(ir: &IR, inf: &Infer, changes: &mut Changes) {
    let mut unread_stores: Set<Stmt> = Set::new();
    for stmt in stmts(ir) {
        if let Statement::Store(_, _, _) = deref_stmt(stmt, ir) {
            unread_stores.insert(stmt);
        }
    }

    for stmt in stmts(ir) {
        let Statement::Compute(_, expr) = deref_stmt(stmt, ir) else { continue; };
        for (_, rt_stack) in specs_of_stmt(stmt, inf) {
            let loc = &inf.local_state[&rt_stack];
            if !loc.executed { continue; }

            let mut handle_read_entry = |entry: &Entry| {
                for x in &entry.sources {
                    unread_stores.remove(x);
                }
            };

            match expr {
                Expr::Index(t, k) => {
                    let t = &loc.nodes[&t];
                    let k = &loc.nodes[&k];
                    let entry = loc.class_states.get_entry(t, k);
                    handle_read_entry(&entry);
                },
                Expr::Next(t, _) | Expr::Len(t) => {
                    let t = &loc.nodes[&t];
                    for cl in &t.classes {
                        for (_, entry) in &loc.class_states.0[cl].0 {
                            handle_read_entry(&entry);
                        }
                    }
                },
                _ => {},
            }
        }
    }

    changes.rm_stmts.extend(unread_stores.iter().copied());
}

// converts const ifs `if cond n1 n2` to `if cond n1 n1` or `if cond n2 n2` if possible.
fn resolve_const_ifs(ir: &IR, inf: &Infer, changes: &mut Changes) {
    for (fid, bid, sid) in stmts(ir) {
        let Statement::If(cond, n1, n2) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if n1 == n2 { continue; }

        let val = specs_of_stmt((fid, bid, sid), inf).iter().map(|(_, rt_stack)| {
            let loc = &inf.local_state[rt_stack];
            if loc.executed {
                loc.nodes[&cond].clone()
            } else {
                Value::bot()
            }
        }).fold(Value::bot(), |x, y| Value::merge(&x, &y));

        if val.bools.len() == 1 {
            let b = *val.bools.iter().next().unwrap();
            let n = if b { n1 } else { n2 };
            let st = Statement::If(cond, n, n);
            changes.change_stmts.insert(((fid, bid, sid), st));
        }
    }
}

fn hollow_uncalled_fns(ir: &IR, inf: &Infer, changes: &mut Changes) {
    let fids: Vec<_> = ir.fns.keys().copied().collect();
    for fid in fids {
        let bid = ir.fns[&fid].start_block;

        // don't repeat, if already hollowed.
        if let Statement::Throw(_) = deref_stmt((fid, bid, 0), ir) { continue; }

        if !stmt_executed((fid, bid, 0), inf) {
            changes.hollow_fns.insert(fid);
        }
    }
}

fn rm_unused_blocks(ir: &IR, inf: &Infer, changes: &mut Changes)  {
    for &fid in ir.fns.keys() {
        for &bid in ir.fns[&fid].blocks.keys() {
            // We don't want to remove start blocks, it might make things inconsistent.
            // These things are resolved by hollow_uncalled_fns.
            if bid == ir.fns[&fid].start_block { continue; }

            if !stmt_executed((fid, bid, 0), inf) {
                changes.rm_blocks.insert((fid, bid));
            }
        }
    }
}
