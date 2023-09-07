use crate::infer::*;

#[derive(Clone, Hash, PartialEq, Eq)]
pub(in crate::infer) enum RtStackElement {
    FnCall(/*call stmt*/ Stmt, FnId),
    Loop(),
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub(in crate::infer) struct RtStack {
    // TODO consider some fancy data structures for this, this is gonna be cloned *a lot*.
    elems: Vec<RtStackElement>,
}


