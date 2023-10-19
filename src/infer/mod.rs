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

type StatementIndex = usize;
type Stmt = (FnId, BlockId, StatementIndex);

// an alloc location
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Location(Stmt);

// A conceptual set of table objects.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum Class {
    Concrete(Location),
    Summary(Location),
}

#[derive(Debug)]
pub struct Infer {
    fn_state: Map<FnId, FnState>,
    dirty: Vec<Stmt>,
}

pub fn infer(ir: &IR) -> Infer {
    let mut inf = Infer {
        fn_state: Map::default(),
        dirty: Vec::new(),
    };

    let fid = ir.main_fn;
    let bid = ir.fns[fid].start_block;

    let mut fstate = FnState::new();
    fstate.state.insert((bid, 0), LocalState::default());

    inf.fn_state.insert(fid, fstate);
    let start_stmt = (fid, bid, 0);
    inf.dirty.push(start_stmt);

    while let Some((fid, bid, sid)) = inf.dirty.pop() {
        let st = &ir.fns[fid].blocks[bid][sid];
        infer_step(st, (fid, bid, sid), &mut inf, ir);
    }

    inf
}
