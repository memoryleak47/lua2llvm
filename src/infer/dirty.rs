use crate::infer::*;
use std::cmp::Ordering;

pub struct Dirty {
    vec: Vec<RtStack>,
}

impl Dirty {
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
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

        // This is a meaningful comparison as we use a bourdoncle block ordering as a pre-optimization.
        if bid < bid_ { return Ordering::Less; }
        if bid > bid_ { return Ordering::Greater ; }

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

