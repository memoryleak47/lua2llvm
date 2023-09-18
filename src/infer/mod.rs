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

struct FnState {
    in_state: LocalState,
    out_state: LocalState,
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

struct TableType {
    state: Set<(Value, Value)>,
}

struct LocalState {
    nodes: Map<Node, Value>,
    table_states: Map<TableClass, TableType>,
}

fn infer(ir: &IR) -> Infer {
    unimplemented!()
}
