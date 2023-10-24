use crate::ir::*;
use hashable::{HashableHashSet as Set, HashableHashMap as Map};
use noisy_float::prelude::{R64, Float};
use std::hash::Hash;

mod display;

mod value;
use value::*;

mod step;
use step::*;

mod class_states;
use class_states::*;

mod local_state;
use local_state::*;

mod fn_state;
use fn_state::*;

pub type StatementIndex = usize;
pub type Stmt = (FnId, BlockId, StatementIndex);

// an alloc location
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Location(Stmt);

// A conceptual set of table objects.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Class {
    Concrete(Location),
    Summary(Location),
}

#[derive(Debug)]
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

    let fid = ir.main_fn;
    let bid = ir.fns[fid].start_block;

    inf.local_state.insert((fid, bid, 0), LocalState::default());
    inf.fn_state.insert(fid, FnState::new());

    inf.dirty.push((fid, bid, 0));

    while let Some((fid, bid, sid)) = inf.dirty.pop() {
        let st = &ir.fns[fid].blocks[bid][sid];
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

