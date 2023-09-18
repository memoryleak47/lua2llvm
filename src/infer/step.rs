use crate::infer::*;

pub(in crate::infer) fn infer_step(st: &Statement, (fid, bid, sid): Stmt, inf: &mut Infer, ir: &IR) {
    let nxt_stmt = || (fid, bid, sid+1);
    match st {
        Statement::Compute(n, expr) => infer_step_compute(*n, expr, (fid, bid, sid), inf),
        Statement::Store(t, i, v) => {
            let mut state: LocalState = inf.fn_state[&fid].state[&(bid, sid)].clone();
            let t: Value = state.nodes[&t].clone();
            let i: Value = state.nodes[&i].clone();
            let v: Value = state.nodes[&v].clone();
            state.class_states.set(&t, &i, &v);
            to_stmt(nxt_stmt(), state, inf);
        },

        Statement::If(cond, then, else_) => {
            unimplemented!()
        },

        Statement::FnCall(f, arg) => {
            unimplemented!()
        },

        Statement::Command(cmd) => {
            match cmd {
                Command::Print(_) => {
                    let current_state = inf.fn_state[&fid].state[&(bid, sid)].clone();
                    to_stmt(nxt_stmt(), current_state, inf);
                }
                Command::Throw(_) => {
                    // nothing to do after this, nothing gets "dirty".
                },
            }
        },

        Statement::Return => {
            unimplemented!()
        },
    }
}

fn infer_step_compute(n: Node, expr: &Expr, (fid, bid, sid): Stmt, inf: &mut Infer) {
    match expr {
        Expr::Index(t, i) => unimplemented!(),
        Expr::Arg => unimplemented!(),
        Expr::NewTable => unimplemented!(),
        Expr::LitFunction(fid) => unimplemented!(),
        Expr::BinOp(kind, n1, n2) => unimplemented!(),
        Expr::Len(n) => unimplemented!(),
        Expr::Intrinsic(i) => unimplemented!(),

        Expr::Num(num) => {
            let mut state: LocalState = inf.fn_state[&fid].state[&(bid, sid)].clone();
            let mut v = Value::bot();
            let num = (*num).try_into().unwrap();
            v.nums = Lattice::Set(vec![num].into_iter().collect());

            state.nodes.insert(n, v);
            to_stmt((fid, bid, sid+1), state, inf);
        },
        Expr::Bool(b) => unimplemented!(),
        Expr::Nil => unimplemented!(),
        Expr::Str(s) => unimplemented!(),
    }
}

fn to_stmt(new: Stmt, state: LocalState, inf: &mut Infer) {
    let (fid, bid, sid) = new;
    let def = LocalState::default();
    let old_state = inf.fn_state[&fid].state.get(&(bid, sid)).unwrap_or(&def);
    let result_state = state.merge(old_state);
    if &result_state != old_state {
        inf.fn_state.get_mut(&fid).unwrap().state.insert((bid, sid), result_state);
        inf.dirty.push(new);
    }
}
