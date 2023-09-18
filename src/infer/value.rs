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

    pub(in crate::infer) fn is_empty(&self) -> bool {
        match self {
            Lattice::Top => false,
            Lattice::Set(s) => s.is_empty(),
        }
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

    pub(in crate::infer) fn compare(&self, other: &Value) -> Comparison {
        [
            compare_lattice(&self.strings, &other.strings),
            compare_set(&self.fns, &other.fns),
            compare_lattice(&self.nums, &other.nums),
            compare_set(&self.nils, &other.nils),
            compare_set(&self.bools, &other.bools),
            compare_set(&self.classes, &other.classes),
        ].iter().fold(Comparison::Disjoint, join_comparison)
    }
}

#[derive(Clone, Copy)]
pub(in crate::infer) enum Comparison {
    ConcreteEq,
    Overlap,
    Disjoint,
}

fn join_comparison(a: Comparison, b: &Comparison) -> Comparison {
    match (a, *b) {
        (Comparison::Disjoint, b_) => b_,
        (a_, Comparison::Disjoint) => a_,
        _ => Comparison::Overlap,
    }
}

fn compare_set<T: Hash + Eq>(a: &Set<T>, b: &Set<T>) -> Comparison {
    let intersection: Set<&T> = a.intersection(b).collect();
    if intersection.is_empty() { return Comparison::Disjoint; }
    if a.len() == 1 && b.len() == 1 {
        return Comparison::ConcreteEq;
    }
    return Comparison::Overlap;
}

fn compare_lattice<T: Hash + Eq>(a: &Lattice<T>, b: &Lattice<T>) -> Comparison {
    match (a, b) {
        (Lattice::Top, _) => {
            if b.is_empty() { Comparison::Disjoint } else { Comparison::Overlap }
        },
        (_, Lattice::Top) => {
            if a.is_empty() { Comparison::Disjoint } else { Comparison::Overlap }
        },
        (Lattice::Set(s1), Lattice::Set(s2)) => compare_set(s1, s2),
    }
}
