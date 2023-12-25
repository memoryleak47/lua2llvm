use std::collections::{HashMap, HashSet};

use crate::ir::*;
use crate::infer::*;
use crate::optimize::util;

#[derive(Clone)]
pub struct Layout {
    pub table_layouts: HashMap<Location, TableLayout>
}

#[derive(Clone)]
pub enum TableLayout {
    HashTable,

    // Each value stored in this Vec corresponds to a field in the resulting struct.
    // The values need to be concrete.
    Struct(Vec<Value>),
}

pub fn layout(ir: &IR, inf: &Infer) -> Layout {
    let mut ly = Layout { table_layouts: HashMap::new() };

    for stmt in util::stmts(ir) {
        handle_spec_stmt(ir, inf, stmt, &mut ly);
    }

    ly
}

fn handle_spec_stmt(ir: &IR, inf: &Infer, stmt: Stmt, ly: &mut Layout) {
    let st = util::deref_stmt(stmt, ir);

    // If two different locations are contained in any Value, we disqualify both of them and fall back to HashTable.
    // The reason we do this is that we don't want to have multiple tables in one runtime value with different layouts.
    fn chk_value(val: &Value, ly: &mut Layout) {
        if single_loc_value(val).is_none() {
            for cl in val.classes.iter() {
                ly.table_layouts.insert(cl.location(), TableLayout::HashTable);
            }
        }
    }

    // We only check the above for nodes, as this is where the collision would eventually arise.
    if let Statement::Compute(n, _) = st {
        let stmt2 = (stmt.0, stmt.1, stmt.2 + 1);
        let v = merged_value(n, stmt2, inf);
        chk_value(&v, ly);
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

        // Having `next` / `len` called on any location disqualifies it from being a struct.
        Statement::Compute(_, Expr::Next(t, _) | Expr::Len(t)) => {
            merged_value(t, stmt, inf).classes.iter()
                .map(|x| x.location())
                .for_each(|l| {
                    ly.table_layouts.insert(l, TableLayout::HashTable);
            });
        },

        // - All its set & get usages have completely inferred concrete non-table keys (bools/nums/strings).
        Statement::Compute(_, Expr::Index(t, idx)) | Statement::Store(t, idx, _) => {
            let Some(loc) = single_loc_value(&merged_value(t, stmt, inf)) else { return; };

            let idx: &Value = &merged_value(idx, stmt, inf);
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

pub fn single_loc_value(value: &Value) -> Option<Location> {
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

pub fn merged_value(t: Node, stmt: Stmt, inf: &Infer) -> Value {
    let mut merged = Value::bot();
    for (_, rtstack) in crate::optimize::util::specs_of_stmt(stmt, &inf) {
        let local_state = &inf.local_state[&rtstack];
        if let Some(v) = local_state.nodes.get(&t) {
            merged = merged.merge(v);
        }
    }

    merged
}

