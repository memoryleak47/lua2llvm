use crate::infer::*;

#[derive(PartialEq, Eq, Hash, Clone)]
pub(in crate::infer) struct Value {
    pub strings: Lattice<String>,
    pub fns: Set<FnId>,
    pub nums: Lattice<R64>, // TODO Lua supports NaN & inf, so we need to do that aswell!
    pub nils: Set<()>,
    pub bools: Set<bool>,
    pub classes: Set<Class>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub(in crate::infer) enum Lattice<T: Eq + Hash> {
    Top,
    Set(Set<T>),
}

impl<T: Hash + Eq> Lattice<T> {
    pub(in crate::infer) fn new() -> Lattice<T> {
        Lattice::Set(Set::new())
    }
}

impl Value {
    pub(in crate::infer) fn merge(&self, other: &Value) -> Value {
        unimplemented!()
    }

    pub(in crate::infer) fn bot() -> Value {
        Value {
            strings: Lattice::new(),
            fns: Set::new(),
            nums: Lattice::new(),
            nils: Set::new(),
            bools: Set::new(),
            classes: Set::new(),
        }
    }

    pub(in crate::infer) fn nil() -> Value {
        let mut v = Value::bot();
        v.nils.insert(());
        v
    }
}
