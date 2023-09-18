use crate::infer::*;

#[derive(Default, PartialEq, Eq, Hash, Clone)]
pub(in crate::infer) struct ClassStates(pub(in crate::infer) Map<Class, ClassState>);

#[derive(Default, PartialEq, Eq, Hash, Clone)]
pub(in crate::infer) struct ClassState(pub(in crate::infer) Map<Value, Value>);

impl ClassStates {
    pub(in crate::infer) fn set(&mut self, t: &Value, k: &Value, v: &Value) {
        if t.classes.len() > 1 {
            for c in &t.classes {
                self.0.get_mut(c).unwrap().weak_set(k, v);
            }
        } else {
            for c in &t.classes {
                self.0.get_mut(c).unwrap().set(k, v);
            }
        }
    }

    pub(in crate::infer) fn get(&self, t: &Value, k: &Value) -> Value {
        let mut out_val = Value::bot();
        for c in &t.classes {
            out_val = out_val.merge(&self.0[c].get(k));
        }
        out_val
    }
}

impl ClassState {
    pub(in crate::infer) fn set(&mut self, k: &Value, v: &Value) {
        if k.is_concrete() {
            self.0.insert(k.clone(), v.clone());
        } else {
            // add the possibilities of `v` to `map[k]`.
            let r = self.0.entry(k.clone()).or_insert(Value::bot());
            *r = r.merge(v);

            // also add the possibilities of `v` to all concrete ones that overlap
            for (k_, v_) in self.0.iter_mut() {
                if k_.is_concrete() && k_.compare(k) == Comparison::Overlap {
                    *v_ = v_.merge(v);
                }
            }
        }
    }

    pub(in crate::infer) fn weak_set(&mut self, k: &Value, v: &Value) {
        let mut a = self.clone();
        a.set(k, v);
        *self = self.merge(&a);
    }

    pub(in crate::infer) fn get(&self, k: &Value) -> Value {
        let mut weak_val = Value::nil();
        for (k_, v) in &self.0 {
            match k_.compare(k) {
                Comparison::ConcreteEq => {
                    return v.clone();
                },
                Comparison::Overlap => {
                    weak_val = weak_val.merge(v);
                },
                Comparison::Disjoint => {},
            }
        }

        weak_val
    }

    pub(in crate::infer) fn merge(&self, other: &ClassState) -> ClassState {
        let mut out = ClassState::default();

        let mut keys: Set<&_> = self.0.keys().collect();
        keys.extend(other.0.keys());

        for k in &keys {
            let v = if k.is_concrete() {
                let l1 = self.get(k);
                let l2 = other.get(k);
                l1.merge(&l2)
            } else {
                let bot = Value::bot();
                let l1 = self.0.get(k).unwrap_or(&bot);
                let l2 = other.0.get(k).unwrap_or(&bot);
                l1.merge(&l2)
            };
            out.0.insert((**k).clone(), v);
        }

        out
    }
}
