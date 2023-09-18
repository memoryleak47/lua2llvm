use crate::infer::*;

#[derive(PartialEq, Eq, Hash)]
pub(in crate::infer) struct Value {
    prim: PrimitiveValue,
    table: TableValue,
}

#[derive(PartialEq, Eq, Hash)]
pub(in crate::infer) struct TableValue(Set<TableClass>);

#[derive(PartialEq, Eq, Hash)]
pub(in crate::infer) enum Lattice<T: Eq + Hash> {
    Top,
    Set(Set<T>),
}

#[derive(PartialEq, Eq, Hash)]
pub(in crate::infer) struct PrimitiveValue {
    str_val: Lattice<String>,
    fn_val: Set<FnId>,
    num_val: Lattice<R64>, // TODO Lua supports NaN & inf, so we need to do that aswell!
    nil_val: Set<()>,
    bool_val: Set<bool>,
}

impl Value {
    pub(in crate::infer) fn nil() -> Value {
        Value {
            prim: PrimitiveValue::nil(),
            table: TableValue(Set::new()),
        }
    }
}

impl PrimitiveValue {
    pub(in crate::infer) fn nil() -> PrimitiveValue {
        PrimitiveValue {
            str_val: Lattice::new(),
            fn_val: Set::new(),
            num_val: Lattice::new(),
            nil_val: Some(()).into_iter().collect(),
            bool_val: Set::new(),
        }
    }
}

impl<T: Hash + Eq> Lattice<T> {
    pub(in crate::infer) fn new() -> Lattice<T> {
        Lattice::Set(Set::new())
    }
}


