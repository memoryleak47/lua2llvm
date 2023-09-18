use crate::infer::*;

pub(in crate::infer) struct FnState {
    pub argval: Value,
    pub out_state: ClassStates,

    // the state right before executing a statement.
    pub state: Map<(BlockId, StatementIndex), LocalState>,
}

impl FnState {
    pub(in crate::infer) fn new() -> FnState {
        FnState {
            argval: Value::bot(),
            out_state: ClassStates::default(),
            state: Map::new(),
        }
    }

    pub(in crate::infer) fn push_call(&mut self, argval: Value, class_states: ClassStates, start_bid: BlockId) {
        self.argval = self.argval.merge(&argval);
        let loc_st: &mut LocalState = self.state.entry((start_bid, 0)).or_insert(LocalState::default());
        loc_st.class_states = loc_st.class_states.merge(&class_states);
    }
}


