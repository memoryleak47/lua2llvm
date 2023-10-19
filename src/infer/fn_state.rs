use crate::infer::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(in crate::infer) struct FnState {
    pub argval: Value,

    // is set to None, if the function never returned yet.
    pub out_state: Option<ClassStates>,

    // the state right before executing a statement.
    pub state: Map<(BlockId, StatementIndex), LocalState>,

    // Stores where this function was called.
    // This is relevant for propagating the return output to all call sites.
    pub call_sites: Set<Stmt>,
}

impl FnState {
    pub(in crate::infer) fn new() -> FnState {
        FnState {
            argval: Value::bot(),
            out_state: None,
            state: Map::new(),
            call_sites: Set::new(),
        }
    }
}
