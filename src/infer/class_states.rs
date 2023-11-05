use crate::infer::*;

#[derive(Default, PartialEq, Eq, Hash, Clone)]
pub struct ClassStates(pub Map<Class, ClassState>);

#[derive(Default, PartialEq, Eq, Hash, Clone)]
pub struct ClassState(pub Map<Value, Entry>);

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Entry {
    pub value: Value,
    pub sources: Set<Stmt>, // the Store statements that shaped this entry.
}

impl ClassStates {
    pub(in crate::infer) fn set(&mut self, t: &Value, k: &Value, v: &Value, source: Stmt) {
        let mut tt = Value::bot();
        tt.classes = t.classes.clone();

        if tt.is_concrete() {
            for c in &tt.classes {
                self.0.get_mut(c).unwrap().set(k, v, source);
            }
        } else {
            for c in &tt.classes {
                self.0.get_mut(c).unwrap().weak_set(k, v, source);
            }
        }
    }

    pub fn get_entry(&self, t: &Value, k: &Value) -> Entry {
        let mut out_entry = Entry::bot();
        for c in &t.classes {
            out_entry = out_entry.merge(&self.0[c].get_entry(k));
        }
        out_entry
    }

    pub fn get(&self, t: &Value, k: &Value) -> Value {
        self.get_entry(t, k).value
    }

    pub fn merge(&self, other: &ClassStates) -> ClassStates {
        let mut out = ClassStates::default();

        let mut classes: Set<&Class> = self.0.keys().collect();
        classes.extend(other.0.keys());

        for cl in &classes {
            let st1 = self.0.get(cl);
            let st2 = other.0.get(cl);
            let st = match (st1, st2) {
                (Some(a), None) => a.clone(),
                (None, Some(b)) => b.clone(),
                (Some(a), Some(b)) => a.merge(&b),
                (None, None) => unreachable!(),
            };
            out.0.insert((**cl).clone(), st);
        }

        out
    }

    pub fn map_classes(&self, f: &impl Fn(Class) -> Class) -> Self {
        let mut out = ClassStates::default();

        for cl in self.0.keys() {
            let mut cl_state = self.0[cl].map_classes(f);
            let cl = f(*cl);
            if out.0.contains_key(&cl) {
                cl_state = out.0[&cl].merge(&cl_state);
            }
            out.0.insert(cl, cl_state);
        }
        out
    }
}

impl ClassState {
    pub fn set(&mut self, k: &Value, v: &Value, source: Stmt) {
        let entry = Entry {
            value: v.clone(),
            sources: vec![source].iter().copied().collect(),
        };
        if k.is_concrete() {
            self.0.insert(k.clone(), entry);
        } else {
            // add the possibilities of `v` to `map[k]`.
            let r = self.0.entry(k.clone()).or_insert(Entry::bot());
            *r = r.merge(&entry);

            // also add the possibilities of `v` to all concrete ones that overlap
            for (k_, entry_) in self.0.iter_mut() {
                if k_.is_concrete() && k_.compare(k) == Comparison::Overlap {
                    *entry_ = entry_.merge(&entry);
                }
            }
        }
    }

    pub fn weak_set(&mut self, k: &Value, v: &Value, source: Stmt) {
        let mut a = self.clone();
        a.set(k, v, source);
        *self = self.merge(&a);
    }

    pub fn get_entry(&self, k: &Value) -> Entry {
        let mut weak_entry = Entry::nil();
        for (k_, entry) in &self.0 {
            match k_.compare(k) {
                Comparison::ConcreteEq => {
                    return entry.clone();
                },
                Comparison::Overlap => {
                    weak_entry = weak_entry.merge(entry);
                },
                Comparison::Disjoint => {},
            }
        }

        weak_entry
    }

    pub fn merge(&self, other: &ClassState) -> ClassState {
        let mut out = ClassState::default();

        let mut keys: Set<&_> = self.0.keys().collect();
        keys.extend(other.0.keys());

        for k in &keys {
            let v = if k.is_concrete() {
                let l1 = self.get_entry(k);
                let l2 = other.get_entry(k);
                l1.merge(&l2)
            } else {
                let bot = Entry::bot();
                let l1 = self.0.get(k).unwrap_or(&bot);
                let l2 = other.0.get(k).unwrap_or(&bot);
                l1.merge(&l2)
            };
            out.0.insert((**k).clone(), v);
        }

        out
    }

    pub fn map_classes(&self, f: &impl Fn(Class) -> Class) -> Self {
        let mut out = ClassState::default();

        for k in self.0.keys() {
            let mut v = self.0[k].map_classes(f);
            let k = k.map_classes(f);
            if out.0.contains_key(&k) {
                v = out.0[&k].merge(&v);
            }
            out.0.insert(k, v);
        }

        out
    }
}

impl Entry {
    pub fn map_classes(&self, f: &impl Fn(Class) -> Class) -> Self {
        Self {
            value: self.value.map_classes(f),
            sources: self.sources.clone(),
        }
    }

    pub fn merge(&self, other: &Entry) -> Self {
        Self {
            value: self.value.merge(&other.value),
            sources: self.sources.union(&other.sources).copied().collect(),
        }
    }

    pub fn bot() -> Self {
        Self {
            value: Value::bot(),
            sources: Set::new(),
        }
    }

    pub fn nil() -> Self {
        Self {
            value: Value::nil(),
            sources: Set::new(),
        }
    }
}

impl Class {
    pub fn allocated_at(&self, stmt: Stmt) -> bool {
        match self {
            Class::Concrete(Location(stmt2)) => stmt2 == &stmt,
            Class::Summary(Location(stmt2)) => stmt2 == &stmt,
        }
    }
}
