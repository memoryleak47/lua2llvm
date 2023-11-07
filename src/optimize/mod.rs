use crate::ir::*;
use crate::infer::*;
use std::hash::Hash;

pub mod util;
use util::*;

mod bourdoncle;
use bourdoncle::*;

#[derive(PartialEq, Eq)]
pub enum Changed { Yes, No }

// TODO re-add Changed argument to check convergence.
type PreOptimization = fn(&mut IR);
type Optimization = fn(&mut IR, &Infer) -> Changed;

static PRE_OPTIMIZATIONS: &'static [PreOptimization] = &[rm_unused_node, rm_unused_fns, merge_blocks, bourdoncle_blocks];
static OPTIMIZATIONS: &'static [Optimization] = &[resolve_const_compute, rm_unread_store, resolve_const_ifs, hollow_uncalled_fns, rm_unused_blocks];

pub fn optimize(ir: &mut IR) {
    let mut inf = reinfer(ir);
    loop {
        let mut changed = Changed::No;
        for o in OPTIMIZATIONS {
            if o(ir, &inf) == Changed::Yes {
                inf = reinfer(ir);
                changed = Changed::Yes;
            }
        }

        if changed == Changed::No { break; }
    }
}

fn reinfer(ir: &mut IR) -> Infer {
    for o in PRE_OPTIMIZATIONS {
        o(ir);
    }
    println!("{}", crate::ir_to_string(ir));

    infer(&ir)
}

// pre opts:

fn rm_unused_node(ir: &mut IR) {
    let mut s = Vec::new();
    for (fid, bid, sid) in stmts(ir) {
        let Statement::Compute(node, _) = deref_stmt((fid, bid, sid), ir) else { continue; };
        if stmts_in_fid(fid, ir).iter().all(|stmt2| !deref_stmt(*stmt2, ir).uses_node(node)) {
            s.push((fid, bid, sid));
        }
    }

    rm_stmts(s, ir);
}

fn rm_unused_fns(ir: &mut IR) {
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

    let unused: Vec<FnId> = ir.fns.keys().copied().filter(|fid| !open.contains(fid)).collect();

    rm_fns(unused, ir);
}

fn merge_blocks(ir: &mut IR) {
    if let Some((fid, (bid1, bid2))) = find_mergeable_blocks(ir) {
        // remove bid1 -> bid2 jump.
        let orig_if_sid = ir.fns[&fid].blocks[&bid1].len() - 1;
        rm_stmt((fid, bid1, orig_if_sid), ir);

        // move over stmts from bid2 to bid1.
        let blks: Vec<Statement> = ir.fns.get_mut(&fid).unwrap().blocks.remove(&bid2).unwrap();
        ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid1).unwrap().extend(blks);
    }
}

fn bourdoncle_blocks(ir: &mut IR) {
    fn reorder_fn(f: &Function, order: &BlockOrder) -> Function {
        Function {
            blocks: f.blocks.iter().map(|(bid, blk)| (order[&bid], reorder_block(blk, order))).collect(),
            start_block: order[&f.start_block],
        }
    }

    fn reorder_block(blk: &[Statement], order: &BlockOrder) -> Vec<Statement> {
        let mut out = Vec::new();
        for s in blk {
            if let Statement::If(cond, b1, b2) = s {
                let b1 = order[&b1];
                let b2 = order[&b2];
                out.push(Statement::If(*cond, b1, b2));
            } else {
                out.push(s.clone());
            }
        }
        out
    }

    let keys: Vec<FnId> = ir.fns.keys().cloned().collect();
    for fid in keys {
        let order = bourdoncle(fid, ir);
        let old_f = &ir.fns[&fid];
        let new_f = reorder_fn(old_f, &order);
        ir.fns.insert(fid, new_f);
    }
}

// opts:

fn resolve_const_compute(ir: &mut IR, inf: &Infer) -> Changed {
    let mut changed = Changed::No;
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
        ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid).unwrap()[sid] = Statement::Compute(node, new_expr);
        changed = Changed::Yes;
    }

    changed
}

// Three reasons why a store might not be optimized out:
// - someone indexes into the table
// - someone uses next on the table
// - someone accesses the length of the table
fn rm_unread_store(ir: &mut IR, inf: &Infer) -> Changed {
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

    if unread_stores.is_empty() {
        Changed::No
    } else {
        rm_stmts(unread_stores.iter().copied().collect(), ir);
        Changed::Yes
    }
}

// converts const ifs `if cond n1 n2` to `if cond n1 n1` or `if cond n2 n2` if possible.
fn resolve_const_ifs(ir: &mut IR, inf: &Infer) -> Changed {
    let mut changed = Changed::No;
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
            ir.fns.get_mut(&fid).unwrap().blocks.get_mut(&bid).unwrap()[sid] = st;

            changed = Changed::Yes;
        }
    }

    changed
}

// replaces the contents of a fn with throw("unreachable!");
fn hollow_fn(fid: FnId, ir: &mut IR) {
    // remove all blocks.
    let blks = ir.fns[&fid].blocks.keys().map(|&bid| (fid, bid)).collect();
    rm_blocks(blks, ir);

    let f = ir.fns.get_mut(&fid).unwrap();
    f.start_block = 0;
    f.blocks.insert(0, vec![Statement::Throw(String::from("unreachable!"))]);
}

fn hollow_uncalled_fns(ir: &mut IR, inf: &Infer) -> Changed {
    let mut changed = Changed::No;
    let fids: Vec<_> = ir.fns.keys().copied().collect();
    for fid in fids {
        let bid = ir.fns[&fid].start_block;

        // don't repeat, if already hollowed.
        if let Statement::Throw(_) = deref_stmt((fid, bid, 0), ir) { continue; }

        if !stmt_executed((fid, bid, 0), inf) {
            changed = Changed::Yes;
            hollow_fn(fid, ir);
        }
    }

    changed
}

// This could also be implemented as a pre-optimization with similar quality.
// We could check which Blocks are reachable from the starting block, and remove all others.
// In combination with resolve_const_ifs this should have the same result, no?
fn rm_unused_blocks(ir: &mut IR, inf: &Infer) -> Changed {
    let mut blocks = Vec::new();

    for &fid in ir.fns.keys() {
        for &bid in ir.fns[&fid].blocks.keys() {
            // We don't want to remove start blocks, it might make things inconsistent.
            // These things are resolved by hollow_uncalled_fns.
            if bid == ir.fns[&fid].start_block { continue; }

            if !stmt_executed((fid, bid, 0), inf) {
                blocks.push((fid, bid));
            }
        }
    }

    if blocks.is_empty() {
        Changed::No
    } else {
        rm_blocks(blocks, ir);
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
