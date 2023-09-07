use crate::ir::*;
use std::collections::{HashSet, HashMap};
use std::hash::Hash;

mod step;
use step::*;

mod merge;
use merge::*;

mod table_state;
use table_state::*;

mod rt_stack;
use rt_stack::*;

type Stmt = (FnId, BlockId, /*statement index*/ usize);

#[derive(Clone, PartialEq, Eq, Hash)]
struct Marker {
    rt_stack: RtStack,
    // number of elements on the rt_stack that are still recent.
    recency_count: usize,
}

#[derive(Clone, PartialEq)]
enum Lattice<T> {
    Set(Vec<T>), // cannot hash f64, hence Vec is chosen.
    // for finite types like T = bool, nil, FnId; the variant Top should never be chosen.
    Top,
}

#[derive(Clone, PartialEq)]
struct PrimitiveState {
    fn_state: Lattice<FnId>,
    bool_state: Lattice<bool>,
    num_state: Lattice<f64>,
    nil_state: Lattice<()>,
    str_state: Lattice<String>,
}

#[derive(Clone, PartialEq)]
struct State {
    primitive_state: PrimitiveState,
    marker_state: HashSet<Marker>,
    // is `Some`, if the corresponding value *might* be a table, `None` otherwise.
    table_state: Option<TableState>,
}

type LocalState = HashMap<Marker, State>;

pub struct Infer {
    // the state of a marker right before executing a particular statement.
    states: HashMap<Stmt, LocalState>,
    dirty: Vec<Stmt>, // these stmts need to be re-evaluated.
}

fn first_statement(ir: &IR) -> Stmt {
    let fid = ir.main_fn;
    let f = &ir.fns[fid];
    let bid = f.start_block;
    (fid, bid, 0)
}

fn eval((fid, bid, sid): Stmt, ir: &IR) -> &Statement {
    &ir.fns[fid].blocks[bid][sid]
}

pub fn infer(ir: &IR) -> Infer {
    let mut inf = Infer {
        states: Default::default(),
        dirty: Default::default(),
    };

    inf.dirty.push(first_statement(ir));

    while let Some(stmt) = inf.dirty.pop() {
        infer_step(stmt, ir, &mut inf);
    }

    inf
}
