use crate::infer::*;

pub(in crate::infer) fn infer_step(st: &Statement, (fid, bid, sid): Stmt, inf: &mut Infer, ir: &IR) {
    let nxt_stmt = || (fid, bid, sid+1);
    match st {
        Statement::Compute(n, expr) => {
            unimplemented!()
        },

        Statement::Store(t, i, v) => {
            unimplemented!()
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

fn to_stmt(new: Stmt, state: LocalState, inf: &mut Infer) {
    let (fid, bid, sid) = new;
    let old_state = &inf.fn_state[&fid].state[&(bid, sid)];
    let result_state = state.merge(old_state);
    if &result_state != old_state {
        inf.fn_state.get_mut(&fid).unwrap().state.insert((bid, sid), result_state);
        inf.dirty.push(new);
    }
}
