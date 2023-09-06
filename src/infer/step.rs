use crate::infer::*;

pub(in crate::infer) fn infer_step(stmt: Stmt, ir: &IR, inf: &mut Infer) {
    match eval(stmt, ir) {
        Statement::Compute(n, expr) => infer_step_compute(*n, expr, stmt, ir, inf),
        Statement::Store(table, index, val) => infer_step_store(*table, *index, *val, stmt, ir, inf),
        Statement::If(cond, then_bid, else_bid) => infer_step_if(*cond, *then_bid, *else_bid, stmt, ir, inf),
        Statement::FnCall(func, arg) => infer_step_fn_call(*func, *arg, stmt, ir, inf),
        Statement::Command(cmd) => infer_step_command(cmd, stmt, ir, inf),
        Statement::Return => {},
    }
}

fn infer_step_compute(n: Node, expr: &Expr, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    unimplemented!()
}

fn infer_step_store(table: Node, index: Node, val: Node, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    unimplemented!()
}

fn infer_step_if(cond: Node, then_bid: BlockId, else_bid: BlockId, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    unimplemented!()
}

fn infer_step_fn_call(func: Node, arg: Node, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    unimplemented!()
}

fn infer_step_command(command: &Command, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    match command {
        Command::Print(_) => mk_next_dirty(stmt, inf),
        Command::Throw(_) => {},
    }
}


fn mk_next_dirty((fid, bid, sid): Stmt, inf: &mut Infer) {
    let next = (fid, bid, sid + 1);
    inf.dirty.push(next);
}
