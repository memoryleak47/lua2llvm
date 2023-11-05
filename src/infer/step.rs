use crate::infer::*;

pub(in crate::infer) fn infer_step(rt_stack: RtStack, inf: &mut Infer, ir: &IR) {
    inf.local_state.get_mut(&rt_stack).unwrap().executed = true;
    let (fid, bid, sid) = rt_stack.last().cloned().unwrap();
    let st = &ir.fns[&fid].blocks[&bid][sid];

    let mut state: LocalState = inf.local_state[&rt_stack].clone();
    match st {
        Statement::Compute(n, expr) => {
            infer_step_compute(*n, expr, rt_stack, inf);
        },
        Statement::Store(t, i, v) => {
            let t: Value = state.nodes[&t].clone();
            let i: Value = state.nodes[&i].clone();
            let v: Value = state.nodes[&v].clone();
            state.class_states.set(&t, &i, &v, (fid, bid, sid));
            jump_to_stmt((bid, sid+1), &rt_stack, state, inf);
        },

        Statement::If(cond, then, else_) => {
            let cond: Value = state.nodes[&cond].clone();
            for (b, jump_bid) in [(true, *then), (false, *else_)] {
                if cond.bools.contains(&b) {
                    jump_to_stmt((jump_bid, 0), &rt_stack, state.clone(), inf);
                }
            }
        },

        Statement::FnCall(f, arg) => {
            let summ_state: LocalState = state.map_classes(&summarize_all);

            let f: Value = summ_state.nodes[&f].clone();
            let arg: Value = summ_state.nodes[&arg].clone();

            for &child_fid in &f.fns {
                let new_spec = jump_to_fn(child_fid, rt_stack.clone(), &summ_state.class_states, &arg, ir, inf);

                // take the current output state of that function as well.
                // TODO this dirty order doesn't seem correct. The dirty.last() should be evaluating the child fn, not proceeding the current fn.
                if let Some(ret_class_states) = &inf.fn_state[&new_spec].out_state {
                    let mut new_state = summ_state.clone();
                    new_state.class_states = new_state.class_states.merge(ret_class_states);
                    jump_to_stmt((bid, sid+1), &rt_stack, new_state, inf);
                }
            }
        },

        Statement::Print(_) => {
            let current_state = inf.local_state[&rt_stack].clone();
            jump_to_stmt((bid, sid+1), &rt_stack, current_state, inf);
        },
        Statement::Throw(_) => {
            // nothing to do after this, nothing gets "dirty".
        },

        Statement::Return => {
            let current = current_spec(rt_stack.clone());
            let st: &mut FnState = inf.fn_state.get_mut(&current).unwrap();
            let new_out: ClassStates = state.class_states.map_classes(&summarize_all);
            let new_out: ClassStates = match &st.out_state {
                Some(x) => x.merge(&new_out),
                None => new_out,
            };
            st.out_state = Some(new_out.clone());
            let call_sites = st.call_sites.clone();

            for site in &call_sites {
                let (_, bid_, sid_) = site.last().cloned().unwrap();
                let mut new_state = inf.local_state[site].map_classes(&summarize_all);
                new_state.class_states = new_state.class_states.merge(&new_out);

                jump_to_stmt((bid_, sid_+1), &site, new_state, inf);
            }
        },
    }
}

fn infer_step_compute(n: Node, expr: &Expr, rt_stack: RtStack, inf: &mut Infer) {
    let mut state: LocalState = inf.local_state[&rt_stack].clone();
    let (fid, bid, sid) = rt_stack.last().cloned().unwrap();

    let mut v = Value::bot();
    match expr {
        Expr::Index(t, i) => {
            let t = &state.nodes[&t];
            let i = &state.nodes[&i];
            v = state.class_states.get(t, i);
        },
        Expr::Arg => {
            let mut s = rt_stack.clone();
            s.pop();

            let spec = FnSpec {
                rt_stack: s,
                fid,
            };
            v = inf.fn_state[&spec].argval.clone();
        },
        Expr::NewTable => {
            let loc = Location((fid, bid, sid));
            state = state.map_classes(&|cl| {
                if cl == Class::Concrete(loc) { return Class::Summary(loc); }
                return cl;
            });
            let cl = Class::Concrete(loc);
            v.classes.insert(cl);
            state.class_states.0.insert(cl, ClassState::default());
        },
        Expr::Function(fid_) => {
            v.fns = vec![*fid_].into_iter().collect();
        },
        Expr::BinOp(kind, l, r) => {
            let l = &state.nodes[&l];
            let r = &state.nodes[&r];
            v = infer_binop(kind, l, r);
        },
        Expr::Len(_) => {
            v.nums = Lattice::Top;
        },
        Expr::Next(t, _) => {
            // `nil` is always an option for next.
            v.nils = vec![()].into_iter().collect();

            let t = &state.nodes[t];
            for cl in &t.classes {
                for k in state.class_states.0[cl].0.keys() {
                    v = v.merge(k);
                }
            }
        },
        Expr::Type(o) => {
            let o = &state.nodes[o];
            let mut outputs = Vec::new();
            if !o.strings.is_empty() { outputs.push("string"); }
            if !o.nums.is_empty() { outputs.push("number"); }
            if !o.nils.is_empty() { outputs.push("nil"); }
            if !o.fns.is_empty() { outputs.push("function"); }
            if !o.classes.is_empty() { outputs.push("table"); }
            if !o.bools.is_empty() { outputs.push("boolean"); }
            v.strings = Lattice::Set(outputs.into_iter().map(|x| x.to_string()).collect());
        },
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
    jump_to_stmt((bid, sid+1), &rt_stack, state, inf);
}

fn infer_binop(kind: &BinOpKind, l: &Value, r: &Value) -> Value {
    let num_attempts = |f: fn(_, _) -> _| {
        let mut out = Value::bot();

        let Lattice::Set(l) = &l.nums else { out.nums = Lattice::Top; return out; };
        let Lattice::Set(r) = &r.nums else { out.nums = Lattice::Top; return out; };

        let mut set = Set::new();
        for l in l {
            for r in r {
                set.insert(f(*l, *r));
            }
        }

        if set.len() > 3 {
            out.nums = Lattice::Top;
        } else {
            out.nums = Lattice::Set(set);
        }

        out
    };

    let cmp_attempts = |f: fn(_, _) -> _| {
        let mut out = Value::bot();

        let top = vec![true, false].into_iter().collect();
        let Lattice::Set(l) = &l.nums else { out.bools = top; return out; };
        let Lattice::Set(r) = &r.nums else { out.bools = top; return out; };

        for l in l {
            for r in r {
                out.bools.insert(f(*l, *r));
            }
        }

        out
    };

    match kind {
        BinOpKind::Plus => num_attempts(|x, y| x + y),
        BinOpKind::Minus => num_attempts(|x, y| x - y),
        BinOpKind::Mul => num_attempts(|x, y| x * y),
        BinOpKind::Div => num_attempts(|x, y| x / y),
        BinOpKind::Mod => num_attempts(|x, y| x % y),
        BinOpKind::Pow => num_attempts(|x, y| x.powf(y)),

        BinOpKind::Lt => cmp_attempts(|x, y| x < y),
        BinOpKind::Gt => cmp_attempts(|x, y| x > y),
        BinOpKind::Le => cmp_attempts(|x, y| x <= y),
        BinOpKind::Ge => cmp_attempts(|x, y| x >= y),

        BinOpKind::Concat => {
            let mut out = Value::bot();
            out.strings = Lattice::Top;

            out
        },

        BinOpKind::IsEqual => {
            let opts = match l.compare(r) {
                Comparison::ConcreteEq => vec![true],
                Comparison::Overlap => vec![true, false],
                Comparison::Disjoint => vec![false],
            };

            let mut out = Value::bot();
            out.bools = opts.into_iter().collect();

            out
        },

        BinOpKind::IsNotEqual => {
            let opts = match l.compare(r) {
                Comparison::ConcreteEq => vec![false],
                Comparison::Overlap => vec![true, false],
                Comparison::Disjoint => vec![true],
            };

            let mut out = Value::bot();
            out.bools = opts.into_iter().collect();

            out
        },
    }
}

fn jump_to_stmt((bid, sid): (BlockId, StatementIndex), prev_rt_stack: &RtStack, state: LocalState, inf: &mut Infer) -> bool {
    let mut rt_stack = prev_rt_stack.clone();
    rt_stack.last_mut().unwrap().1 = bid;
    rt_stack.last_mut().unwrap().2 = sid;

    jump_to(rt_stack, state, inf)
}

fn jump_to(rt_stack: RtStack, state: LocalState, inf: &mut Infer) -> bool {
    let old_state = &inf.local_state[&rt_stack];
    let result_state = state.merge(old_state);
    if &result_state != old_state {
        inf.local_state.insert(rt_stack.clone(), result_state);
        inf.dirty.push(rt_stack);

        true
    } else { false }
}

// called when we are at `call_site` and we call the function `fid`.
fn jump_to_fn(fid: FnId, call_site: RtStack, class_states: &ClassStates, argval: &Value, ir: &IR, inf: &mut Infer) -> FnSpec {
    let bid = ir.fns[&fid].start_block;
    let new_spec = mk_spec(call_site.clone(), fid);

    let mut start_rt_stack = new_spec.rt_stack.clone();
    start_rt_stack.push((fid, bid, 0));

    inf.init_spec(&new_spec, &ir);

    // set the LocalState accordingly.
    let mut loc = LocalState::default();
    loc.class_states = class_states.clone();
    let set_to_dirty = jump_to(start_rt_stack.clone(), loc, inf);

    // set the FnState accordingly.
    let fn_state: &mut FnState = inf.fn_state.get_mut(&new_spec).unwrap();
    let new_argval = fn_state.argval.merge(argval);
    let needs_update = /*old argval*/ fn_state.argval != new_argval;

    fn_state.argval = new_argval;
    fn_state.call_sites.insert(call_site);

    if needs_update && !set_to_dirty {
        inf.dirty.push(start_rt_stack);
    }

    new_spec
}

fn mk_spec(mut rt_stack: RtStack, fid: FnId) -> FnSpec {
    let pos = rt_stack.iter().position(|&(fid_, _, _)| fid_ == fid);
    if let Some(i) = pos {
        rt_stack.truncate(i);
    }

    FnSpec { rt_stack, fid }
}

fn current_spec(mut rt_stack: RtStack) -> FnSpec {
    let (fid, _bid, _sid) = rt_stack.pop().unwrap();
    FnSpec { rt_stack, fid }
}

fn summarize_all(c: Class) -> Class {
    match c {
        Class::Concrete(x) => Class::Summary(x),
        Class::Summary(x) => Class::Summary(x),
    }
}
