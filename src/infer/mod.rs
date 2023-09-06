use crate::ir::*;
use std::collections::{HashSet, HashMap};

mod step;
use step::*;

type Stmt = (FnId, BlockId, /*statement index*/ usize);

struct Marker {
    location: Stmt,
    is_summary: bool,
}

enum Lattice<T> {
    Set(HashSet<T>),
    Top,
}

struct PrimitiveState {
    fn_state: Lattice<FnId>,
    bool_state: Lattice<bool>,
    num_state: Lattice<f64>,
    nil_state: Lattice<()>,
    str_state: Lattice<String>,
}

struct State {
    primitive_state: PrimitiveState,
    marker_state: HashSet<Marker>,
    table_state: Option<HashSet<(Marker, Marker)>>,
}

pub struct Infer {
    // the state of a marker right before executing a particular statement.
    states: HashMap<Stmt, HashMap<Marker, State>>,
    dirty: Vec<Stmt>, // these stmts need to be re-evaluated.
}

fn find_alloc_locations(ir: &IR) -> Vec<Stmt> {
    unimplemented!()
}

fn calc_markers(ir: &IR) -> Vec<Marker> {
    let mut out = Vec::new();
    for x in find_alloc_locations(ir) {
        for b in [true, false] {
            out.push(Marker { location: x, is_summary: b });
        }
    }
    out
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
