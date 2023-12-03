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

// a Location `l` is Struct-layoutable, if
// - All its set & get usages have completely inferred concrete non-table keys (bools/nums/strings).
// - There exists no `Value` in the infer that contains *l/@l and additionally any other table. (Neither a Node-Value, nor a Value in the ClassStates)
// - Next is never applied on @l|*l.
pub fn layout(ir: &IR, inf: &Infer) -> Layout {
    // TODO
    Layout { table_layouts: HashMap::new() }
}
