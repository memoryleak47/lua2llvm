use crate::ir::*;
use std::collections::{HashSet as Set, HashMap as Map};

type StatementIndex = usize;
type Stmt = (FnId, BlockId, StatementIndex);

// an alloc location
struct Location(Stmt);

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

#[derive(Default)]
struct TableState(Map<TableClass, TableType>);

struct FnState {
    in_state: InState,
    out_state: TableState,
    state: Map<(BlockId, StatementIndex), LocalState>,
}

struct Value {
    prim: PrimitiveValue,
    table: TableValue,
}

struct TableValue(Set<TableClass>);

enum Lattice<T> {
    Top,
    Set(Set<T>),
}

struct PrimitiveValue {
    str_val: Lattice<String>,
    fn_val: Set<FnId>,
    num_val: Lattice<f64>,
    nil_val: Set<()>,
    bool_val: Set<bool>,
}

struct TableType(Set<(Value, Value)>);

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
        infer_step(st, &mut inf, ir);
    }

    inf
}

fn infer_step(st: &Statement, inf: &mut Infer, ir: &IR) {
    unimplemented!()
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

impl Value {
    fn nil() -> Value {
        Value {
            prim: PrimitiveValue::nil(),
            table: TableValue(Set::new()),
        }
    }
}

impl PrimitiveValue {
    fn nil() -> PrimitiveValue {
        PrimitiveValue {
            str_val: Lattice::new(),
            fn_val: Set::new(),
            num_val: Lattice::new(),
            nil_val: Some(()).into_iter().collect(),
            bool_val: Set::new(),
        }
    }
}

impl<T> Lattice<T> {
    fn new() -> Lattice<T> {
        Lattice::Set(Set::new())
    }
}
