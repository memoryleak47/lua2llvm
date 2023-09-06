use crate::infer::*;

pub(in crate::infer) fn infer_step(stmt: Stmt, ir: &IR, inf: &mut Infer) {
    let stmt = eval(stmt, ir);
    match stmt {
        Statement::Compute(n, expr) => infer_step_compute(*n, expr, ir, inf),
        _ => unimplemented!(),
    }
}

fn infer_step_compute(n: Node, expr: &Expr, ir: &IR, inf: &mut Infer) {
    unimplemented!()
}

