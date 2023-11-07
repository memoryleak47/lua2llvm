use std::collections::{HashMap, HashSet};
use crate::ir::*;

pub type BlockOrder = HashMap<BlockId, usize>;
type EdgeSet = HashMap<BlockId, HashSet<BlockId>>;

pub fn bourdoncle_blocks(ir: &mut IR) {
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

fn bourdoncle(fid: FnId, ir: &IR) -> BlockOrder {
    let mut outs = HashMap::new();
    let mut ins = HashMap::new();

    for &v in ir.fns[&fid].blocks.keys() {
        outs.insert(v, HashSet::new());
        ins.insert(v, HashSet::new());
    }

    for (u, v) in edges(fid, ir) {
        outs.get_mut(&u).unwrap().insert(v);
        ins.get_mut(&v).unwrap().insert(u);
    }

    let vertices: HashSet<BlockId> = ir.fns[&fid].blocks.keys().cloned().collect();

    bourdoncle_impl(vertices, &ins, &outs)
}


// This algorithm could be a bit faster by using tarjans algorithm.
// But I think it shouldn't be a bottleneck.
fn bourdoncle_impl(mut vertices: HashSet<BlockId>, ins: &EdgeSet, outs: &EdgeSet) -> BlockOrder {
    if vertices.is_empty() {
        return BlockOrder::new();
    }
    if vertices.len() == 1 {
        let mut order = BlockOrder::new();
        order.insert(*vertices.iter().next().unwrap(), 0);
        return order;
    }

    let (a, b) = match find_cut(&vertices, ins, outs) {
        Some((a, b)) => (a, b),
        None => {
            let h = choose_h(&vertices, ins, outs);
            vertices.remove(&h);

            let mut h_set = HashSet::new();
            h_set.insert(h);

            (h_set, vertices)
        },
    };

    return merge_orders(bourdoncle_impl(a, ins, outs), bourdoncle_impl(b, ins, outs));
}

// if this returns Some((A, B)), then
// - A and B are a partition of the vertex set
// - A and B are both non-empty
// - there are no edges from B to A.
fn find_cut(vertices: &HashSet<BlockId>, ins: &EdgeSet, outs: &EdgeSet) -> Option<(HashSet<BlockId>, HashSet<BlockId>)> {
    let v = *vertices.iter().min().unwrap(); // we use min() to make it deterministic.

    // a = set of vertices that reach v.
    let a = recurse(v, vertices, ins);
    if a.len() != vertices.len() {
        let b_ = vertices.difference(&a).cloned().collect();
        return Some((a, b_));
    }

    // b = set of vertices reachable from v.
    let b = recurse(v, vertices, outs);
    if b.len() != vertices.len() {
        let a_ = vertices.difference(&b).cloned().collect();
        return Some((a_, b));
    }

    None
}

// returns all vertices reachable by edges in G[vertices].
fn recurse(v: BlockId, vertices: &HashSet<BlockId>, edges: &EdgeSet) -> HashSet<BlockId> {
    let mut todo = HashSet::new();
    let mut done = HashSet::new();

    todo.insert(v);
    while !todo.is_empty() {
        let x = *todo.iter().next().unwrap();
        todo.remove(&x);
        done.insert(x);

        for y in edges[&x].iter() {
            if !done.contains(y) && vertices.contains(y) {
                todo.insert(*y);
            }
        }
    }

    done
}

// moves the numbers of b up by the amount of vertices in a.
fn merge_orders(mut a: BlockOrder, b: BlockOrder) -> BlockOrder {
    let n = a.len();
    for (bid, val) in b.into_iter() {
        let val = val + n;
        a.insert(bid, val);
    }

    a
}

fn choose_h(vertices: &HashSet<BlockId>, ins: &EdgeSet, #[allow(unused)] outs: &EdgeSet) -> BlockId {
    // we look for some vertex with an ingoing edge from the outside of vertices.
    // we use min to make it deterministic.
    if let Some(v) = vertices.iter().filter(|x| !ins[&x].is_subset(vertices)).min() {
        return *v;
    }

    // fallback:
    *vertices.iter().min().unwrap()
}

fn edges(fid: FnId, ir: &IR) -> Vec<(BlockId, BlockId)> {
    let mut e = Vec::new();
    for (bid, statements) in ir.fns[&fid].blocks.iter() {
        let Some(Statement::If(_, bid1, bid2)) = statements.last() else { continue; };
        e.push((*bid, *bid1));
        if bid1 != bid2 {
            e.push((*bid, *bid2));
        }
        
    }
    e
}
