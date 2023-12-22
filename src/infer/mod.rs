use crate::ir::*;
pub use hashable::{HashableHashSet as Set, HashableHashMap as Map};
use noisy_float::prelude::{R64, Float};
use std::hash::Hash;

mod value;
pub use value::*;

mod dirty;
pub use dirty::*;

mod step;
use step::*;

mod class_states;
pub use class_states::*;

mod local_state;
pub use local_state::*;

mod fn_state;
pub use fn_state::*;

pub type StatementIndex = usize;
pub type Stmt = (FnId, BlockId, StatementIndex);

// an alloc location
#[derive(PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct Location(pub Stmt);

// A conceptual set of table objects.
#[derive(PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub enum Class {
    Concrete(Location),
    Summary(Location),
}

// all except for the last statements have to refer to a fn-call.
pub type RtStack = Vec<Stmt>;

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct FnSpec {
    pub rt_stack: RtStack,
    pub fid: FnId,
}

#[derive(Clone)]
pub struct Infer {
    pub fn_state: Map<FnSpec, FnState>,
    pub local_state: Map<RtStack, LocalState>,
    dirty: Dirty,
}

pub fn infer(ir: &IR) -> Infer {
    let mut inf = Infer {
        fn_state: Map::default(),
        dirty: Dirty::init(ir),
        local_state: Map::default(),
    };

    let fid = ir.main_fn;
    let bid = ir.fns[&fid].start_block;

    inf.init_spec(&FnSpec { rt_stack: vec![], fid }, ir);
    inf.dirty.push(vec![(fid, bid, 0)]);

    while let Some(rt_stack) = inf.dirty.pop() {
        infer_step(rt_stack, &mut inf, ir);
    }

    inf
}

impl Class {
    pub fn location(&self) -> Location {
        match self {
            Class::Concrete(l) => *l,
            Class::Summary(l) => *l,
        }
    }
}
