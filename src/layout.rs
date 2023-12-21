use std::collections::{HashMap, HashSet};

use crate::ir::*;
use crate::infer::*;
use crate::optimize::util;

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
    let mut ly = Layout { table_layouts: HashMap::new() };
    for stmt in util::stmts(ir) {
        for (_, rt_stack) in util::specs_of_stmt(stmt, inf) {
            handle_spec_stmt(ir, inf, &rt_stack, stmt, &mut ly);
        }
    }

    ly
}

fn handle_spec_stmt(ir: &IR, inf: &Infer, rt_stack: &RtStack, stmt: Stmt, ly: &mut Layout) {
    let st = util::deref_stmt(stmt, ir);
    let state = &inf.local_state[rt_stack];
    if !state.executed { return; }

    // If two different locations are contained in any Value, we disqualify both of them and fall back to HashTable.
    // The reason we do this is that we don't want to have multiple tables in one runtime value with different layouts.
    fn chk_value(val: &Value, ly: &mut Layout) {
        let locs: HashSet<_> = val.classes.iter().map(|x| x.location()).collect();
        if locs.len() > 1 {
            for loc in locs {
                ly.table_layouts.insert(loc, TableLayout::HashTable);
            }
        }
    }

    // We only check the above for nodes, as this is where the probably would eventually arise.
    for (_, val) in &state.nodes {
        chk_value(val, ly);
    }

    // Every table starts off by trying to be a struct.
    fn init_if_missing(loc: Location, ly: &mut Layout) {
        if !ly.table_layouts.contains_key(&loc) {
            ly.table_layouts.insert(loc, TableLayout::Struct(Vec::new()));
        }
    }

    match st {

        Statement::Compute(_, Expr::NewTable) => {
            let loc = Location(stmt);
            init_if_missing(loc, ly);
        },

        // Having `next` called on any location disqualifies it from being a struct.
        Statement::Compute(_, Expr::Next(t, _)) => {
            state.nodes[&t].classes.iter()
                .map(|x| x.location())
                .for_each(|l| {
                    ly.table_layouts.insert(l, TableLayout::HashTable);
            });
        },

        // - All its set & get usages have completely inferred concrete non-table keys (bools/nums/strings).
        Statement::Compute(_, Expr::Index(t, idx)) | Statement::Store(t, idx, _) => {
            let Some(loc) = single_loc_value(&state.nodes[&t]) else { return; };

            let idx: &Value = &state.nodes[&idx];
            let good_idx = idx.is_concrete() && idx.classes.is_empty();
            if good_idx {
                init_if_missing(loc, ly);
                if let TableLayout::Struct(vals) = ly.table_layouts.get_mut(&loc).unwrap() {
                    if !vals.contains(idx) {
                        vals.push(idx.clone());
                    }
                }
            } else {
                // location disqualified!
                ly.table_layouts.insert(loc, TableLayout::HashTable);
            }
        },

        _ => {},
    }
}

fn single_loc_value(value: &Value) -> Option<Location> {
    // `value` is only allowed to store classes, nothing else.
    let mut copy = Value::bot();
    copy.classes = value.classes.clone();
    if &copy != value { return None; }

    let mut locs = HashSet::new();
    for cl in &value.classes {
        locs.insert(cl.location());
    }
    if locs.len() != 1 { return None; }
    locs.into_iter().next()
}
