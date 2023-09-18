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

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum TableClass {
    Concrete(Location),
    Summary(Location),
}

struct Infer {
    fn_state: Map<FnId, FnState>,
    dirty: Vec<Stmt>,
}

struct InState {
    argval: Value,
    table_state: TableState,
}

#[derive(Default, PartialEq, Eq, Hash, Clone)]
struct TableState(Map<TableClass, TableType>);

struct FnState {
    in_state: InState,
    out_state: TableState,

    // the state right before executing a statement.
    state: Map<(BlockId, StatementIndex), LocalState>,
}


#[derive(Default, PartialEq, Eq, Hash, Clone)]
struct TableType(Set<(Value, Value)>);

#[derive(PartialEq, Eq, Clone)]
struct LocalState {
    nodes: Map<Node, Value>,
    table_state: TableState,
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
            table_state: TableState::default(),
        }
    }
}

impl LocalState {
    fn merge(&self, other: &LocalState) -> LocalState {
        unimplemented!()
    }
}
