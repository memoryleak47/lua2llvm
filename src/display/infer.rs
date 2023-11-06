use crate::infer::*;
use crate::display::*;
use std::fmt::{self, Formatter, Display};

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Location((fid, bid, sid)) = self;
        write!(f, "({}, {}, {})", fid, bid, sid)
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Class::Concrete(x) => write!(f, "@{}", x),
            Class::Summary(x) => write!(f, "*{}", x),
        }
    }
}

impl Display for Value {
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

        parts.extend(self.classes.iter().map(|x| x.to_string()));

        write!(f, "{}", parts.join("|"))
    }
}

impl Display for ClassState {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        for (k, v) in self.0.iter() {
            parts.push(format!("{}: {}", k, v.value));
        }
        write!(f, "[{}],", parts.join(", "))
    }
}

impl Display for ClassStates {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (class, class_state) in ordered_map_iter(self.0.iter()) {
            write!(f, "    - {}: ", class)?;
            write!(f, "{}", class_state)?;
            write!(f, "\n")?;
        }
        write!(f, "\n")?;

        Ok(())
    }
}

impl Display for LocalState {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "------------------------------------\n")?;
        write!(f, "{}", self.class_states)?;

        for (node, value) in ordered_map_iter(self.nodes.iter()) {
            write!(f, "    {} = {},\n", node_string(*node), value)?;
        }
        write!(f, "\n")?;

        if !self.executed {
            write!(f, "    never executed.\n")?;
        }

        write!(f, "------------------------------------\n")?;
        Ok(())
    }
}


pub fn display_rt_stack(rt_stack: &RtStack, f: &mut Formatter<'_>) -> fmt::Result {
    for (fid, bid, sid) in rt_stack {
        write!(f, "/({fid}, {bid}, {sid})")?;
    }
    Ok(())
}

impl Display for FnSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        display_rt_stack(&self.rt_stack, f)?;
        write!(f, "/{}", self.fid)
    }
}
