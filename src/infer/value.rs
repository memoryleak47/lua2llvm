use crate::infer::*;

#[derive(PartialEq, Eq, Hash, Clone)]
pub(in crate::infer) struct Value {
    strings: Lattice<String>,
    fns: Set<FnId>,
    nums: Lattice<R64>, // TODO Lua supports NaN & inf, so we need to do that aswell!
    nils: Set<()>,
    bools: Set<bool>,
    classes: Set<Class>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub(in crate::infer) enum Lattice<T: Eq + Hash> {
    Top,
    Set(Set<T>),
}

impl Value {
    pub(in crate::infer) fn nil() -> Value {
        Value {
            strings: Lattice::new(),
            fns: Set::new(),
            nums: Lattice::new(),
            nils: Some(()).into_iter().collect(),
            bools: Set::new(),
            classes: Set::new(),
        }
    }
}

impl<T: Hash + Eq> Lattice<T> {
    pub(in crate::infer) fn new() -> Lattice<T> {
        Lattice::Set(Set::new())
    }
}
