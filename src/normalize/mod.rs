use crate::ir::*;

pub mod bourdoncle;
use bourdoncle::*;

// This function makes an IR more readable by ordering it.
pub fn normalize(ir: &mut IR) {
    normalize_blocks(ir);
    // we could later also normalize nodes. They tend to grow pretty large as well.
}

fn normalize_blocks(ir: &mut IR) {
    fn reorder_fn(f: &Function, order: &BlockOrder) -> Function {
        Function {
            blocks: f.blocks.iter().map(|(bid, blk)| (order[&bid], reorder_block(blk, order))).collect(),
            start_block: order[&f.start_block],
        }
    }

    fn reorder_block(blk: &[Statement], order: &BlockOrder) -> Vec<Statement> {
        let mut out = Vec::new();
        for s in blk {
            if let Statement::If(cond, b1, b2) = s {
                let b1 = order[&b1];
                let b2 = order[&b2];
                out.push(Statement::If(*cond, b1, b2));
            } else {
                out.push(s.clone());
            }
        }
        out
    }

    let keys: Vec<FnId> = ir.fns.keys().cloned().collect();
    for fid in keys {
        let order = bourdoncle(fid, ir);
        let old_f = &ir.fns[&fid];
        let new_f = reorder_fn(old_f, &order);
        ir.fns.insert(fid, new_f);
    }
}
