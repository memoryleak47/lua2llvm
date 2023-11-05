use crate::infer::*;

#[derive(Clone, PartialEq, Eq)]
// TODO rename to FnSpecState or something.
pub struct FnState {
    pub argval: Value,

    // is set to None, if the function never returned yet.
    pub out_state: Option<ClassStates>,

    // Stores where this function was called.
    // This is relevant for propagating the return output to all call sites.
    pub call_sites: Set<RtStack>,
}

impl FnState {
    pub(in crate::infer) fn new() -> FnState {
        FnState {
            argval: Value::bot(),
            out_state: None,
            call_sites: Set::new(),
        }
    }
}

impl Infer {
    pub fn init_spec(&mut self, spec: &FnSpec, ir: &IR) {
        if self.fn_state.contains_key(&spec) {
            return;
        }

        self.fn_state.insert(spec.clone(), FnState::new());

        let FnSpec { rt_stack, fid } = spec;
        for &bid in ir.fns[&fid].blocks.keys() {
            for sid in 0..ir.fns[&fid].blocks[&bid].len() {
                let mut stack2 = rt_stack.clone();
                stack2.push((*fid, bid, sid));
                self.local_state.insert(stack2, LocalState::default());
            }
        }
    }
}
