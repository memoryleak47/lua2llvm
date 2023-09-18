use crate::ir::*;
use hashable::{HashableHashSet as Set, HashableHashMap as Map};
use noisy_float::prelude::R64;
use std::hash::Hash;

mod value;
use value::*;

mod step;
use step::*;

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

struct Infer {
    fn_state: Map<FnId, FnState>,
    dirty: Vec<Stmt>,
}

struct InState {
    argval: Value,
    class_states: ClassStates,
}

#[derive(Default, PartialEq, Eq, Hash, Clone)]
struct ClassStates(Map<Class, ClassState>);

#[derive(Default, PartialEq, Eq, Hash, Clone)]
struct ClassState(Set<(Value, Value)>);

struct FnState {
    in_state: InState,
    out_state: ClassStates,

    // the state right before executing a statement.
    state: Map<(BlockId, StatementIndex), LocalState>,
}


#[derive(PartialEq, Eq, Clone)]
struct LocalState {
    nodes: Map<Node, Value>,
    class_states: ClassStates,
}

fn infer(ir: &IR) -> Infer {
    let mut inf = Infer {
        fn_state: Map::default(),
        dirty: Vec::new(),
    };

    let fid = ir.main_fn;
    inf.fn_state.insert(fid, FnState::new(InState::nil()));
    let start_stmt = (fid, ir.fns[fid].start_block, 0);
    inf.dirty.push(start_stmt);

    while let Some((fid, bid, sid)) = inf.dirty.pop() {
        let st = &ir.fns[fid].blocks[bid][sid];
        infer_step(st, (fid, bid, sid), &mut inf, ir);
    }

    inf
}

impl FnState {
    fn new(in_state: InState) -> FnState {
        FnState {
            in_state,
            out_state: Default::default(),
            state: Map::new(),
        }
    }
}

impl InState {
    fn nil() -> InState {
        InState {
            argval: Value::nil(),
            class_states: ClassStates::default(),
        }
    }
}

impl LocalState {
    fn merge(&self, other: &LocalState) -> LocalState {
        unimplemented!()
    }
}
