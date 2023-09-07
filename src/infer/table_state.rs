use crate::infer::*;

#[derive(Clone, PartialEq)]
pub(in crate::infer) struct TableState {
    set: HashSet<(Marker, Marker)>,
}

fn merge_table_state(ts1: &TableState, ts2: &TableState) -> TableState {
    unimplemented!()
}

impl TableState {
    fn get(&self, index: Marker) -> Vec<Marker> {
        unimplemented!()
    }

    fn set(&mut self, index: Marker, val: Marker) {
        unimplemented!()
    }

    fn weak_set(&mut self, index: Marker, val: Marker) {
        let mut other = self.clone();
        other.set(index, val);
        *self = merge_table_state(self, &other);
    }
}
