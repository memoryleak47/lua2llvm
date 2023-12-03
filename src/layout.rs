use std::collections::HashMap;

use crate::ir::*;
use crate::infer::*;

pub struct Layout {
    pub table_layouts: HashMap<Location, TableLayout>
}

pub enum TableLayout {
    HashTable,

    // Each value stored in this Vec corresponds to a field in the resulting struct.
    // The values need to be concrete.
    Struct(Vec<Value>),
}

pub fn layout(ir: &IR, inf: &Infer) -> Layout {
    // TODO
    Layout { table_layouts: HashMap::new() }
}
