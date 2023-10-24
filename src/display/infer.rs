use crate::infer::*;
use crate::display::*;
use std::fmt::{self, Formatter};

impl<'ir, 'inf> FnDisplayObj<'ir, 'inf> {
    pub fn value_string(&self, v: &Value) -> String {
        let mut parts: Vec<String> = Vec::new();

        match &v.strings {
            Lattice::Top => parts.push("string".to_string()),
            Lattice::Set(s) => parts.extend(s.iter().map(|x| String::from("\"") + x + "\"")), // we should escape " here.
        }

        parts.extend(v.fns.iter().map(|x| format!("f{}", x)));

        match &v.nums {
            Lattice::Top => parts.push("number".to_string()),
            Lattice::Set(s) => parts.extend(s.iter().map(|x| format!("{}", x))),
        }

        if !v.nils.is_empty() { parts.push(String::from("nil")); }

        parts.extend(v.bools.iter().map(|x| format!("{}", x)));

        parts.extend(v.classes.iter().map(|x| self.class_string(x)));

        parts.join("|")
    }

    pub fn location_string(&self, Location((fid, bid, sid)): Location) -> String {
        format!("({fid}, {bid}, {sid})")
    }

    pub fn class_string(&self, class: &Class) -> String {
        match class {
            Class::Concrete(x) => format!("@{}", self.location_string(*x)),
            Class::Summary(x) => format!("*{}", self.location_string(*x)),
        }
    }

    pub fn display_local_state(&self, local_state: &LocalState, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "------------------------------------\n")?;
        write!(f, "    class states:\n")?;
        self.display_class_states(&local_state.class_states, f)?;

        write!(f, "    node states:\n")?;
        write!(f, "    ")?;
        for (node, value) in &local_state.nodes {
            write!(f, "{} = {}; ", self.node_string(*node), self.value_string(value))?;
        }
        write!(f, "\n\n")?;
        write!(f, "    executed: {}\n", local_state.executed)?;
        write!(f, "------------------------------------\n")?;
        Ok(())
    }

    pub fn display_class_states(&self, class_states: &ClassStates, f: &mut Formatter<'_>) -> fmt::Result {
        for (class, class_state) in class_states.0.iter() {
            write!(f, "    - {}: ", self.class_string(class))?;
            self.display_class_state(class_state, f)?;
            write!(f, "\n")?;
        }
        write!(f, "\n")?;

        Ok(())
    }

    pub fn display_class_state(&self, class_state: &ClassState, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        for (k, v) in class_state.0.iter() {
            parts.push(format!("{}: {}", self.value_string(k), self.value_string(v)));
        }
        write!(f, "[{}]", parts.join(", "))
    }
}
