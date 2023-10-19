use crate::infer::*;
use std::fmt::{self, Debug, Formatter};

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({},{},{})", self.0.0, self.0.1, self.0.2)
    }
}

impl Debug for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Class::Concrete(x) => write!(f, "@{:?}", x),
            Class::Summary(x) => write!(f, "*{:?}", x),
        }
    }

}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parts: Vec<String> = Vec::new();

        match &self.strings {
            Lattice::Top => parts.push("string".to_string()),
            Lattice::Set(s) => parts.extend(s.iter().map(|x| String::from("\"") + x + "\"")), // we should escape " here.
        }

        parts.extend(self.fns.iter().map(|x| format!("f{}", x)));

        match &self.nums {
            Lattice::Top => parts.push("number".to_string()),
            Lattice::Set(s) => parts.extend(s.iter().map(|x| format!("{}", x))),
        }

        if !self.nils.is_empty() { parts.push(String::from("nil")); }

        parts.extend(self.bools.iter().map(|x| format!("{}", x)));

        parts.extend(self.classes.iter().map(|x| format!("{:?}", x)));

        write!(f, "{}", parts.join("|"))
    }
}

impl Debug for ClassState {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        for (k, v) in self.0.iter() {
            parts.push(format!("{:?}: {:?}", k, v));
        }
        write!(f, "[{}]", parts.join(", "))
    }
}
