use crate::infer::*;

#[derive(PartialEq, Eq, Clone, Default)]
pub(in crate::infer) struct LocalState {
    pub nodes: Map<Node, Value>,
    pub class_states: ClassStates,
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
        }
    }

    pub(in crate::infer) fn map_classes(&self, f: &impl Fn(Class) -> Class) -> Self {
        LocalState {
            nodes: self.nodes.into_iter().map(|(k, v)| (*k, v.map_classes(f))).collect(),
            class_states: self.class_states.map_classes(f),
        }
    }
}

