use crate::infer::*;
use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;

type BlockOrder = HashMap<BlockId, usize>;

pub struct Dirty {
    vec: Vec<RtStack>,
    block_orders: HashMap<FnId, BlockOrder>,
}

impl Dirty {
    pub fn init(ir: &IR) -> Self {
        let mut block_orders = HashMap::new();
        for &fid in ir.fns.keys() {
            block_orders.insert(fid, gen_block_order(fid, ir));
        }
        Self {
            vec: Vec::new(),
            block_orders,
        }
    }

    pub fn push(&mut self, x: RtStack) {
        // We reverse this, so that "small" / "early" RtStacks are at a high index and are popped first.
        match self.vec.binary_search_by(|y| self.cmp_rt_stacks(&x, y).reverse()) {
            Ok(_) => {/* it's already in the vector! */},
            Err(i) => {
                self.vec.insert(i, x);
            }
        }
    }

    pub fn pop(&mut self) -> Option<RtStack> {
        self.vec.pop()
    }

    // private impls

    // Returns Ordering::Less, if (fid, bid, sid) comes before (fid_, bid_, sid_).
    fn cmp_stmt(&self, (fid, bid, sid): Stmt, (fid_, bid_, sid_): Stmt) -> Ordering {
        if fid < fid_ { return Ordering::Less; }
        if fid > fid_ { return Ordering::Greater; }

        let order: &BlockOrder = &self.block_orders[&fid];
        let ord_bid = order[&bid];
        let ord_bid_ = order[&bid_];

        if ord_bid < ord_bid_ { return Ordering::Less; }
        if ord_bid > ord_bid_ { return Ordering::Greater ; }

        if sid < sid_ { return Ordering::Less; }
        if sid > sid_ { return Ordering::Greater ; }

        Ordering::Equal
    }

    // Returns Ordering::Less, if s1 comes before s2.
    fn cmp_rt_stacks(&self, s1: &RtStack, s2: &RtStack) -> Ordering {
        let n = s1.len().max(s2.len());
        for i in 0..n {
            match (s1.get(i), s2.get(i)) {
                (Some(stmt1), Some(stmt2)) => {
                    let ord = self.cmp_stmt(*stmt1, *stmt2);
                    if !ord.is_eq() { return ord; }
                }
                (Some(_), None) => return Ordering::Greater,
                (None, Some(_)) => return Ordering::Less,
                (None, None) => unreachable!(),
            }
        }

        Ordering::Equal
    }
}

type EdgeSet = HashMap<BlockId, HashSet<BlockId>>;
fn gen_block_order(fid: FnId, ir: &IR) -> BlockOrder {
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

    bourdoncle(vertices, &ins, &outs)
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

// if this returns Some((A, B)), then
// - A and B are a partition of the vertex set
// - A and B are both non-empty
// - there are no edges from B to A.
fn find_cut(vertices: &HashSet<BlockId>, ins: &EdgeSet, outs: &EdgeSet) -> Option<(HashSet<BlockId>, HashSet<BlockId>)> {
    let v = *vertices.iter().next().unwrap();

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
    if let Some(v) = vertices.iter().find(|x| !ins[&x].is_subset(vertices)) {
        return *v;
    }

    // fallback:
    *vertices.iter().next().unwrap()
}

// This algorithm could be a bit faster by using tarjans algorithm.
// But I think it shouldn't be a bottleneck.
fn bourdoncle(mut vertices: HashSet<BlockId>, ins: &EdgeSet, outs: &EdgeSet) -> BlockOrder {
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

    return merge_orders(bourdoncle(a, ins, outs), bourdoncle(b, ins, outs));
}
