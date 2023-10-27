use crate::infer::*;

#[derive(PartialEq, Eq, Clone, Default)]
pub struct LocalState {
    pub nodes: Map<Node, Value>,
    pub class_states: ClassStates,
    pub executed: bool,
}

impl LocalState {
    pub(in crate::infer) fn merge(&self, other: &LocalState) -> LocalState {
        let mut nn: Set<&Node> = self.nodes.keys().collect();
        nn.extend(other.nodes.keys());

        let mut nodes = Map::new();
        for n in &nn {
            let bot = Value::bot();
            let v1 = self.nodes.get(n).unwrap_or(&bot);
            let v2 = other.nodes.get(n).unwrap_or(&bot);
            let v = v1.merge(&v2);
            nodes.insert(**n, v);
        }

        let class_states = self.class_states.merge(&other.class_states);

        LocalState {
            nodes,
            class_states,
            executed: self.executed || other.executed,
        }
    }

    pub fn map_classes(&self, f: &impl Fn(Class) -> Class) -> Self {
        LocalState {
            nodes: self.nodes.into_iter().map(|(k, v)| (*k, v.map_classes(f))).collect(),
            class_states: self.class_states.map_classes(f),
            executed: self.executed,
        }
    }

    pub fn map_stmt(&self, f: &impl Fn(Stmt) -> Stmt) -> Self {
        LocalState {
            nodes: self.nodes.into_iter().map(|(k, v)| (*k, v.map_stmt(f))).collect(),
            class_states: self.class_states.map_stmt(f),
            executed: self.executed,
        }
    }

    pub fn erase_stmt(&self, f: FnId, stmt: Stmt, ir: &IR) -> Self {
        let mut opt_node = crate::optimize::util::get_node_from_stmt(stmt, ir);
        // if the node belongs to another function than this localstate, then we don't care about it.
        if f != stmt.0 { opt_node = None; }
        LocalState {
            nodes: self.nodes.iter().filter(|(x, _)| Some(**x) != opt_node).map(|(k, v)| (*k, v.erase_stmt(stmt))).collect(),
            class_states: self.class_states.erase_stmt(stmt),
            executed: self.executed,
        }
    }
}

