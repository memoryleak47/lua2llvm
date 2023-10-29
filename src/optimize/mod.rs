use crate::ir::*;
use crate::infer::*;
use std::hash::Hash;

pub mod util;
use util::*;

#[derive(PartialEq, Eq)]
pub enum Changed { Yes, No }
type Optimization = fn(&mut IR, &mut Infer) -> Changed;

static OPTIMIZATIONS: &'static [Optimization] = &[rm_unused_node, resolve_const_compute, rm_unread_store, rm_unused_fns, resolve_const_ifs, hollow_uncalled_fns, rm_unused_blocks, merge_blocks];

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
    let mut s = Vec::new();
    for (fid, bid, sid) in stmts(ir) {
        let Statement::Compute(node, _) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if stmts_in_fid(fid, ir).iter().all(|stmt2| !deref_stmt(*stmt2, ir).uses_node(node)) {
            s.push((fid, bid, sid));
        }
    }

    return rm_stmts(s, ir, inf);
}

fn resolve_const_compute(ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut changed = Changed::No;
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
        changed = Changed::Yes;
    }

    changed
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

// converts const ifs `if cond n1 n2` to `if cond n1 n1` or `if cond n2 n2` if possible.
fn resolve_const_ifs(ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut changed = Changed::No;
    for (fid, bid, sid) in stmts(ir) {
        if !inf.local_state[&(fid, bid, sid)].executed { continue; };
        let Statement::If(cond, n1, n2) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if n1 == n2 { continue; }

        let val = inf.local_state[&(fid, bid, sid)].nodes[&cond].clone();
        if val.bools.len() == 1 {
            let b = *val.bools.iter().next().unwrap();
            let n = if b { n1 } else { n2 };
            let st = Statement::If(cond, n, n);
            ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid).unwrap()[sid] = st;

            changed = Changed::Yes;
        }
    }

    changed
}

// replaces the contents of a fn with throw("unreachable!");
fn hollow_fn(fid: FnId, ir: &mut IR, inf: &mut Infer) {
    // remove all blocks.
    let blks = ir.fns[&fid].blocks.keys().map(|&bid| (fid, bid)).collect();
    rm_blocks(blks, ir, inf);

    let f = ir.fns.get_mut(&fid).unwrap();
    f.start_block = 0;
    f.blocks.insert(0, vec![Statement::Command(Command::Throw(String::from("unreachable!")))]);
    inf.local_state.insert((fid, 0, 0), LocalState::default());
}

fn hollow_uncalled_fns(ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut changed = Changed::No;
    let fids: Vec<_> = ir.fns.keys().copied().collect();
    for fid in fids {
        let start_bid = ir.fns[&fid].start_block;
        if inf.local_state[&(fid, start_bid, 0)].executed { continue; }
        // don't repeat, if already hollowed.
        if let Statement::Command(Command::Throw(_)) = deref_stmt((fid, start_bid, 0), ir) { continue; }

        changed = Changed::Yes;
        hollow_fn(fid, ir, inf);
    }

    changed
}

fn rm_unused_blocks(ir: &mut IR, inf: &mut Infer) -> Changed {
    let mut blocks = Vec::new();

    for &fid in ir.fns.keys() {
        for &bid in ir.fns[&fid].blocks.keys() {
            // We don't want to remove start blocks, it might make things inconsistent.
            // These things are resolved by hollow_uncalled_fns.
            if bid == ir.fns[&fid].start_block { continue; }

            if !inf.local_state[&(fid, bid, 0)].executed {
                blocks.push((fid, bid));
            }
        }
    }

    if blocks.is_empty() {
        Changed::No
    } else {
        rm_blocks(blocks, ir, inf);
        Changed::Yes
    }
}

fn out_blocks(fid: FnId, bid: BlockId, ir: &IR) -> Vec<BlockId> {
    if let Some(Statement::If(_, then_bid, else_bid)) = ir.fns[&fid].blocks[&bid].last() {
        // filter duplicates!
        if then_bid == else_bid { vec![*then_bid] }
        else { vec![*then_bid, *else_bid] }
    } else { vec![] }
}


fn find_mergeable_blocks(ir: &IR) -> Option<(FnId, (BlockId, BlockId))> {
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
                return Some((fid, (bid1, bid2)));
            }
        }
    }
    None
}

fn merge_blocks(ir: &mut IR, inf: &mut Infer) -> Changed {
    if let Some((fid, (bid1, bid2))) = find_mergeable_blocks(ir) {
        // remove bid1 -> bid2 jump.
        let orig_if_sid = ir.fns[&fid].blocks[&bid1].len() - 1;
        rm_stmt((fid, bid1, orig_if_sid), ir, inf);

        // move over stmts from bid2 to bid1.
        let blks: Vec<Statement> = ir.fns.get_mut(&fid).unwrap().blocks.remove(&bid2).unwrap();
        ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid1).unwrap().extend(blks);

        let f = |(fid_, bid_, sid_)| {
            if (fid_, bid_) == (fid, bid2) {
                (fid, bid1, orig_if_sid +  sid_)
            } else { (fid_, bid_, sid_) }
        };
        *inf = inf.map_stmt(&f);
        Changed::Yes
    } else { Changed::No }
}
