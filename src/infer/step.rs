use crate::infer::*;

pub(in crate::infer) fn infer_step(st: &Statement, (fid, bid, sid): Stmt, inf: &mut Infer, ir: &IR) {
    let mut state: LocalState = inf.fn_state[&fid].state[&(bid, sid)].clone();
    match st {
        Statement::Compute(n, expr) => {
            infer_step_compute(*n, expr, &mut state, inf);
            to_stmt((fid, bid, sid+1), state, inf);
        },
        Statement::Store(t, i, v) => {
            let t: Value = state.nodes[&t].clone();
            let i: Value = state.nodes[&i].clone();
            let v: Value = state.nodes[&v].clone();
            state.class_states.set(&t, &i, &v);
            to_stmt((fid, bid, sid+1), state, inf);
        },

        Statement::If(cond, then, else_) => {
            let cond: Value = state.nodes[&cond].clone();
            for (b, jump_bid) in [(true, *then), (false, *else_)] {
                if cond.bools.contains(&b) {
                    to_stmt((fid, jump_bid, 0), state.clone(), inf);
                }
            }
        },

        Statement::FnCall(f, arg) => {
            unimplemented!()
        },

        Statement::Command(cmd) => {
            match cmd {
                Command::Print(_) => {
                    let current_state = inf.fn_state[&fid].state[&(bid, sid)].clone();
                    to_stmt((fid, bid, sid+1), current_state, inf);
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

fn infer_step_compute(n: Node, expr: &Expr, state: &mut LocalState, inf: &mut Infer) {
    let mut v = Value::bot();
    match expr {
        Expr::Index(t, i) => unimplemented!(),
        Expr::Arg => unimplemented!(),
        Expr::NewTable => unimplemented!(),
        Expr::LitFunction(fid) => unimplemented!(),
        Expr::BinOp(kind, n1, n2) => unimplemented!(),
        Expr::Len(n) => unimplemented!(),
        Expr::Intrinsic(i) => unimplemented!(),

        Expr::Num(num) => {
            let num = (*num).try_into().unwrap();
            v.nums = Lattice::Set(vec![num].into_iter().collect());
        },
        Expr::Bool(b) => {
            v.bools = vec![*b].into_iter().collect();
        },
        Expr::Nil => {
            v.nils = vec![()].into_iter().collect();
        },
        Expr::Str(s) => {
            v.strings = Lattice::Set(vec![s.to_string()].into_iter().collect());
        },
    }
    state.nodes.insert(n, v);
}

fn to_stmt((fid, bid, sid): Stmt, state: LocalState, inf: &mut Infer) {
    let def = LocalState::default();
    let old_state = inf.fn_state[&fid].state.get(&(bid, sid)).unwrap_or(&def);
    let result_state = state.merge(old_state);
    if &result_state != old_state {
        inf.fn_state.get_mut(&fid).unwrap().state.insert((bid, sid), result_state);
        inf.dirty.push((fid, bid, sid));
    }
}
