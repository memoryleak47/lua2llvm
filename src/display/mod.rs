mod ir;
use ir::*;

mod infer;

use crate::ir::*;
use crate::infer::*;

use std::fmt::{self, Display, Formatter};

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (&fid, _) in ordered_map_iter(self.fns.iter()) {
            display_fn_header(fid, self, f)?;
            for (&bid, _) in ordered_map_iter(self.fns[&fid].blocks.iter()) {
                display_block_header(fid, bid, self, f)?;
                for st in &self.fns[&fid].blocks[&bid] {
                    write!(f, "{}", st)?;
                }
            }
            display_fn_footer(f)?;
        }

        Ok(())
    }
}

pub fn infer_to_string(ir: &IR, inf: &Infer) -> String {
    let mut out = String::new();
    let f = &mut Formatter::new(&mut out);

    let res: fmt::Result = try {
        for (spec, state) in ordered_map_iter(inf.fn_state.iter()) {
            let fid = spec.fid;

            write!(f, "SPEC {} with arg {}:\n", spec, state.argval)?;
            if let Some(x) = &state.out_state {
                write!(f, "OUT:\n")?;
                write!(f, "{}\n", x)?;
            }

            display_fn_header(fid, ir, f)?;
            for (&bid, _) in ordered_map_iter(ir.fns[&fid].blocks.iter()) {
                display_block_header(fid, bid, ir, f)?;
                for sid in 0..ir.fns[&fid].blocks[&bid].len() {
                    let mut rt_stack = spec.rt_stack.clone();
                    rt_stack.push((fid, bid, sid));
                    write!(f, "{}", inf.local_state[&rt_stack])?;
                    write!(f, "{}", &ir.fns[&fid].blocks[&bid][sid])?;
                }
            }
            display_fn_footer(f)?;
        }
    };
    res.unwrap();

    out
}

fn ordered_map_iter<'s, K: Ord + 's, V: 's>(it: impl Iterator<Item=(&'s K, &'s V)>,) -> impl Iterator<Item=(&'s K, &'s V)> {
    let mut v: Vec<_> = it.collect();
    v.sort_by_key(|x| x.0);
    v.into_iter()
}
