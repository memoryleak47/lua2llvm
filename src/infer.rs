use crate::ir::*;
use std::collections::{HashSet, HashMap};

type Stmt = (FnId, BlockId, /*statement index*/ usize);

struct Marker {
    location: Stmt,
    is_summary: bool,
}

struct PrimitiveState {
    fn_state: HashSet<FnId>,
    bool_state: HashSet<bool>,
    num_state: HashSet<f64>,
    nil_state: HashSet<()>,
    str_state: HashSet<String>,
}

struct State {
    primitive_state: PrimitiveState,
    marker_state: HashSet<Marker>,
    table_state: Option<HashSet<(Marker, Marker)>>,
}

struct Infer {
   states: HashMap<(Stmt, Marker), State>,
}
