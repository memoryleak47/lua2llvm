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
    for bid in [then_bid, else_bid] {
        let st = (stmt.0, bid, 0);
        let old = inf.states.get(&stmt).cloned().unwrap_or(Default::default());
        call_stmt(st, old, inf);
    }
}

// TODO where is the Arg-type of a function stored? probably in Infer.
// is `fn_arg_ty: Marker` / `fn_arg_ty: PrimitiveType / Table` fine?
// actually, we need to find all relevant tables that can be putten into here.
fn infer_step_fn_call(func: Node, arg: Node, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    unimplemented!()
}

fn infer_step_command(command: &Command, stmt: Stmt, ir: &IR, inf: &mut Infer) {
    match command {
        Command::Print(_) => finish_stmt(stmt, inf.states[&stmt].clone(), inf),
        Command::Throw(_) => {},
    }
}


fn finish_stmt((fid, bid, sid): Stmt, new_state: LocalState, inf: &mut Infer) {
    let next = (fid, bid, sid + 1);
    call_stmt(next, new_state, inf);
}

fn call_stmt(stmt: Stmt, new_state: LocalState, inf: &mut Infer) {
    let old = inf.states.get(&stmt).cloned().unwrap_or(Default::default());
    let merged = merge_local_state(&old, &new_state);
    if inf.states.get(&stmt) != Some(&merged) {
        inf.states.insert(stmt, merged);
        inf.dirty.push(stmt);
    }
}
