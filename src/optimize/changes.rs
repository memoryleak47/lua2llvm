use crate::ir::*;
use crate::optimize::*;

#[derive(Default)]
pub struct Changes {
    pub rm_fns: Set<FnId>,
    pub hollow_fns: Set<FnId>, // maybe I don't add this for now?
    pub rm_blocks: Set<(FnId, BlockId)>,
    pub change_stmts: Set<(Stmt, Statement)>,
    pub rm_stmts: Set<Stmt>,
    pub merge_blocks: Set<(FnId, BlockId, BlockId)>,
}

impl Changes {
    pub fn is_empty(&self) -> bool {
        self.rm_fns.is_empty() &&
        self.hollow_fns.is_empty() &&
        self.rm_blocks.is_empty() &&
        self.change_stmts.is_empty() &&
        self.rm_stmts.is_empty() &&
        self.merge_blocks.is_empty()
    }

    pub fn apply(mut self, ir: &mut IR) {
        for fid in take(&mut self.rm_fns).iter() {
            ir.fns.remove(fid);
            self.on_rm_fn(*fid);
        }

        for fid in take(&mut self.hollow_fns).iter() {
            hollow_fn(*fid, ir);
            self.on_hollow_fn(*fid);
        }

        for (fid, bid) in take(&mut self.rm_blocks).iter() {
            ir.fns.get_mut(fid).unwrap().blocks.remove(bid);
            self.on_rm_block(*fid, *bid);
        }

        for ((fid, bid, sid), st) in take(&mut self.change_stmts).iter() {
            ir.fns.get_mut(fid).unwrap().blocks.get_mut(bid).unwrap()[*sid] = st.clone();
        }

        // higher stmts should be rmeoved first, so that we don't mix up the indices.
        let mut stmts: Vec<Stmt> = take(&mut self.rm_stmts).iter().copied().collect();
        stmts.sort_by_key(|&(_, _, sid)| sid);
        stmts.reverse();
        for (fid, bid, sid) in stmts.iter() {
            ir.fns.get_mut(fid).unwrap().blocks.get_mut(bid).unwrap().remove(*sid);
        }

        while let Some(&(fid, bid1, bid2)) = self.merge_blocks.iter().find(|(fid, bid1, bid2)| self.merge_blocks.iter().all(|(fid_, bid1_, bid2_)| (fid, bid1, bid2) == (fid_, bid1_, bid2_) || fid != fid_ || bid2 != bid1_)) {
            self.merge_blocks.remove(&(fid, bid1, bid2));
            merge_block(fid, bid1, bid2, ir);
        }
    }

    fn on_rm_fn(&mut self, fid: FnId) {
        self.hollow_fns.retain(|&fid_| fid_ != fid);
        self.on_hollow_fn(fid);
    }

    fn on_hollow_fn(&mut self, fid: FnId) {
        self.rm_blocks.retain(|&(fid_, _)| fid_ != fid);
        self.change_stmts.retain(|&((fid_, _, _), _)| fid_ != fid);
        self.rm_stmts.retain(|&(fid_, _, _)| fid_ != fid);
        self.merge_blocks.retain(|&(fid_, _, _)| fid_ != fid);
    }

    fn on_rm_block(&mut self, fid: FnId, bid: BlockId) {
        self.change_stmts.retain(|&((fid_, bid_, _), _)| (fid_, bid_) != (fid, bid));
        self.rm_stmts.retain(|&(fid_, bid_, _)| (fid_, bid_) != (fid, bid));
        self.merge_blocks.retain(|&(fid_, bid1_, bid2_)| fid_ != fid && bid1_ != bid && bid2_ != bid);
    }
}

fn take<T: Default>(t1: &mut T) -> T {
    let mut t2 = T::default();
    std::mem::swap(t1, &mut t2);

    t2
}

fn merge_block(fid: FnId, bid1: BlockId, bid2: BlockId, ir: &mut IR) {
    let bbs = &mut ir.fns.get_mut(&fid).unwrap().blocks;
    // remove bid1 -> bid2 jump.

    // remove the final if from bid1.
    bbs.get_mut(&bid1).unwrap().pop();

    // move over stmts from bid2 to bid1.
    let stmts: Vec<Statement> = bbs.remove(&bid2).unwrap();
    bbs.get_mut(&bid1).unwrap().extend(stmts);
}

// replaces the contents of a fn with throw("unreachable!");
fn hollow_fn(fid: FnId, ir: &mut IR) {
    let f = ir.fns.get_mut(&fid).unwrap();
    f.blocks.clear();
    f.start_block = 0;
    f.blocks.insert(0, vec![Statement::Throw(String::from("unreachable!"))]);
}

