use crate::infer::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(in crate::infer) struct FnState {
    pub argval: Value,

    // is set to None, if the function never returned yet.
    pub out_state: Option<ClassStates>,

    // Stores where this function was called.
    // This is relevant for propagating the return output to all call sites.
    pub call_sites: Set<Stmt>,
}

impl FnState {
    pub(in crate::infer) fn new() -> FnState {
        FnState {
            argval: Value::bot(),
            out_state: None,
            call_sites: Set::new(),
        }
    }

    pub(in crate::infer) fn map_stmt(&self, f: &impl Fn(Stmt) -> Stmt) -> Self {
        Self {
            argval: self.argval.map_stmt(f),
            out_state: self.out_state.as_ref().map(|x| x.map_stmt(f)),
            call_sites: self.call_sites.iter().copied().map(f).collect(),
        }
    }
}
