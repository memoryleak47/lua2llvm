use std::cmp::Ordering;
use std::collections::HashMap;

use crate::infer::*;

mod bourdoncle;
use bourdoncle::*;

type BlockOrder = HashMap<BlockId, usize>;

pub struct Dirty {
    vec: Vec<RtStack>,
    block_orders: HashMap<FnId, BlockOrder>,
}

impl Dirty {
    pub fn init(ir: &IR) -> Self {
        let mut block_orders = HashMap::new();
        for &fid in ir.fns.keys() {
            block_orders.insert(fid, bourdoncle(fid, ir));
        }

        Self {
            vec: Vec::new(),
            block_orders,
        }
    }

    pub fn push(&mut self, x: RtStack) {
        // We want "small" / "early" RtStacks to be at a high index, so that they are popped first.
        match self.vec.binary_search_by(|y| self.cmp_rt_stacks(&x, y)) {
            Ok(_) => {/* it's already in the vector! */},
            Err(i) => {
                self.vec.insert(i, x);
            }
        }

        // If you want to ensure that order is correct, simply do the following:
        // dbg!(&self.vec);
    }

    pub fn pop(&mut self) -> Option<RtStack> {
        self.vec.pop()
    }

    // private impls

    // Returns Ordering::Less, if (fid, bid, sid) comes before (fid_, bid_, sid_).
    fn cmp_stmt(&self, (fid, bid, sid): Stmt, (fid_, bid_, sid_): Stmt) -> Ordering {
        if fid < fid_ { return Ordering::Less; }
        if fid > fid_ { return Ordering::Greater; }

        let order = &self.block_orders[&fid];
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

