use crate::ir::*;
pub use hashable::{HashableHashSet as Set, HashableHashMap as Map};
use noisy_float::prelude::{R64, Float};
use std::hash::Hash;

mod value;
pub use value::*;

mod step;
use step::*;

mod class_states;
pub use class_states::*;

mod local_state;
pub use local_state::*;

mod fn_state;
pub use fn_state::*;

pub type StatementIndex = usize;
pub type Stmt = (FnId, BlockId, StatementIndex);

// an alloc location
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Location(pub Stmt);

// A conceptual set of table objects.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Class {
    Concrete(Location),
    Summary(Location),
}

pub struct Infer {
    pub fn_state: Map<FnId, FnState>,
    pub local_state: Map<Stmt, LocalState>,
    dirty: Vec<Stmt>,
}

pub fn infer(ir: &IR) -> Infer {
    let mut inf = Infer {
        fn_state: Map::default(),
        dirty: Vec::new(),
        local_state: Map::default(),
    };

    // initialize everything, so we can index without care later on.
    for fid in 0..ir.fns.len() {
        inf.fn_state.insert(fid, FnState::new());
        for bid in 0..ir.fns[&fid].blocks.len() {
            for sid in 0..ir.fns[&fid].blocks[&bid].len() {
                inf.local_state.insert((fid, bid, sid), LocalState::default());
            }
        }
    }

    let fid = ir.main_fn;
    let bid = ir.fns[&fid].start_block;
    inf.dirty.push((fid, bid, 0));

    while let Some((fid, bid, sid)) = inf.dirty.pop() {
        let st = &ir.fns[&fid].blocks[&bid][sid];
        infer_step(st, (fid, bid, sid), &mut inf, ir);
    }

    inf
}

impl Infer {
    pub fn map_stmt(&self, f: &impl Fn(Stmt) -> Stmt) -> Self {
        Self {
            fn_state: self.fn_state.iter().map(|(fid, state)| (*fid, state.map_stmt(f))).collect(),
            dirty: self.dirty.iter().copied().map(f).collect(),
            local_state: self.local_state.iter().map(|(stmt, local)| (f(*stmt), local.map_stmt(f))).collect(),
        }
    }

    pub fn erase_stmt(&self, stmt: Stmt, ir: &IR) -> Self {
        Self {
            fn_state: self.fn_state.iter().map(|(fid, state)| (*fid, state.erase_stmt(stmt))).collect(),
            dirty: self.dirty.iter().copied().filter(|x| x != &stmt).collect(),
            local_state: self.local_state.iter().filter(|(stmt2, _)| **stmt2 != stmt).map(|(stmt2, local)| (*stmt2, local.erase_stmt(stmt2.0, stmt, ir))).collect(),
        }
    }
}

impl Class {
    fn map_stmt(&self, f: &impl Fn(Stmt) -> Stmt) -> Self {
        match self {
            Self::Concrete(loc) => Self::Concrete(loc.map_stmt(f)),
            Self::Summary(loc) => Self::Summary(loc.map_stmt(f)),
        }
    }
}

impl Location {
    fn map_stmt(&self, f: &impl Fn(Stmt) -> Stmt) -> Self {
        Self(f(self.0))
    }
}

