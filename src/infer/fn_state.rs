use crate::infer::*;

pub(in crate::infer) struct FnState {
    pub argval: Value,
    pub out_state: ClassStates,

    // the state right before executing a statement.
    pub state: Map<(BlockId, StatementIndex), LocalState>,
    pub call_sites: Map<Stmt, CallSiteMap>,
}

// maps `parents` Classes to `childs` Classes.
#[derive(PartialEq, Eq)]
pub(in crate::infer) struct CallSiteMap(Map<Class, Class>);

impl FnState {
    pub(in crate::infer) fn new() -> FnState {
        FnState {
            argval: Value::bot(),
            out_state: ClassStates::default(),
            state: Map::new(),
            call_sites: Map::new(),
        }
    }

    pub(in crate::infer) fn register_call_site(&mut self, argval: &Value, class_states: &ClassStates, site: Stmt, start_bid: BlockId) -> /*something changed*/ bool {
        let (cs_map, child_class_states) = build_call_site_map(argval, class_states);

        let loc_st: &mut LocalState = self.state.entry((start_bid, 0)).or_insert(LocalState::default());

        let mut changed = false;

        let new_class_states = loc_st.class_states.merge(&child_class_states);
        if loc_st.class_states != new_class_states {
            changed = true;
            loc_st.class_states = new_class_states;
        }

        let new_argval = self.argval.merge(argval);
        if self.argval != new_argval {
            changed = true;
            self.argval = new_argval;
        }

        if self.call_sites.get(&site) != Some(&cs_map) {
            changed = true;
            self.call_sites.insert(site, cs_map);
        }

        changed
    }
}

fn build_call_site_map(argval: &Value, class_states: &ClassStates) -> (CallSiteMap, ClassStates) {
    let mapper = |x| {
        match x {
            Class::Concrete(y) => Class::Summary(y),
            Class::Summary(y) => Class::Summary(y),
        }
    };

    let reachable = reachable_classes(argval, class_states);

    let mut csmap = Map::new();
    let mut child_class_states = ClassStates::default();

    for &c in &reachable {
        let child_c = mapper(c);
        csmap.insert(c, child_c);

        let child_class_state = class_states.0[&c].map_classes(&mapper);
        child_class_states.0.insert(child_c, child_class_state);
    }

    (CallSiteMap(csmap), child_class_states)
}

fn reachable_classes(val: &Value, class_states: &ClassStates) -> Set<Class> {
    // `todo` is the subset of `out`, which has not yet been checked.
    let mut out: Set<Class> = Set::new();
    let mut todo: Set<Class> = Set::new();

    fn add_todo_val(val: &Value, todo: &mut Set<Class>, out: &mut Set<Class>) {
        for c in &val.classes {
            if !out.contains(c) {
                todo.insert(*c);
                out.insert(*c);
            }
        }
    }

    add_todo_val(val, &mut todo, &mut out);

    while !todo.is_empty() {
        let x = *todo.iter().next().unwrap();
        todo.remove(&x);

        for (k, v) in class_states.0[&x].0.iter() {
            add_todo_val(k, &mut todo, &mut out);
            add_todo_val(v, &mut todo, &mut out);
        }
    }

    out
}
