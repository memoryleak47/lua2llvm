mod fobj;
use fobj::FnDisplayObj;

mod infer;

use crate::ir::*;
use crate::infer::*;

use std::collections::HashMap;
use std::fmt::{self, Formatter};

const INLINE_CONST_NODES: bool = false;

pub fn ir_to_string(ir: &IR, inf: Option<&Infer>) -> String {
    let mut out = String::new();
    let mut f = Formatter::new(&mut out);

    fmt_ir(ir, inf, &mut f).unwrap();

    out
}

fn fmt_ir(ir: &IR, inf: Option<&Infer>, f: &mut Formatter<'_>) -> fmt::Result {
    for (&fid, _) in ordered_map_iter(&ir.fns) {
        let mut fobj = FnDisplayObj::new(ir, inf);
        fobj.display_fn(fid, f)?;
    }

    Ok(())
}

fn ordered_map_iter<K: Ord, V>(map: &HashMap<K, V>) -> impl Iterator<Item=(&K, &V)> + '_ {
    let mut v: Vec<_> = map.iter().collect();
    v.sort_by_key(|x| x.0);
    v.into_iter()
}
