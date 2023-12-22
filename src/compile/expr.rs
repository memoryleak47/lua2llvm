use super::*;

pub fn compile_expr(e: &Expr, ctxt: &mut Ctxt) -> ll::ValueId {
    match e {
        Expr::Nil => mk_nil(ctxt),
        Expr::Bool(b) => {
            let bool_t = ctxt.bool_t();
            let b = ctxt.push_compute(ll::Expr::ConstInt(bool_t, *b as _));

            mk_bool(b, ctxt)
        }
        Expr::Num(x) => {
            let f64_t = ctxt.f64_t();
            let x = ctxt.push_compute(ll::Expr::ConstReal(f64_t, x.const_raw()));

            mk_num(x, ctxt)
        },
        Expr::Str(s) => {
            let def = ll::GlobalDef::String(s.to_string());
            let gid = ctxt.alloc_global(def);
            let v = ll::ValueId::Global(gid);

            mk_str(v, ctxt)
        },
        Expr::NewTable => {
            let var = alloc(ctxt);
            call_extra_fn("new_table", &[var], ctxt);

            load_val(var, ctxt)
        }
        Expr::Function(fid) => {
            let fid = ctxt.lit_fns[fid];
            let fid = ll::ValueId::Global(fid);
            mk_fn(fid, ctxt)
        },
        Expr::Index(t, i) => {
            let t = alloc_val(ctxt.nodes[t], ctxt);
            let i = alloc_val(ctxt.nodes[i], ctxt);
            let out = alloc(ctxt);
            call_extra_fn("table_get", &[t, i, out], ctxt);
            load_val(out, ctxt)
        },
        Expr::Arg => {
            let param = ctxt.push_compute(ll::Expr::Arg(0));
            load_val(param, ctxt)
        },
        Expr::BinOp(k, l, r) => compile_binop(*k, l, r, ctxt),
        Expr::Len(n) => {
            let n = ctxt.nodes[n];
            let n = alloc_val(n, ctxt);

            let out = alloc(ctxt);
            call_extra_fn("len", &[n, out], ctxt);
            load_val(out, ctxt)
        },
        Expr::Type(v) => {
            let v = alloc_val(ctxt.nodes[v], ctxt);
            let t = alloc(ctxt);
            call_extra_fn("type", &[v, t], ctxt);

            load_val(t, ctxt)
        },
        Expr::Next(v1, v2) => {
            let v1 = alloc_val(ctxt.nodes[v1], ctxt);
            let v2 = alloc_val(ctxt.nodes[v2], ctxt);
            let t = alloc(ctxt);
            call_extra_fn("next", &[v1, v2, t], ctxt);

            load_val(t, ctxt)
        },
    }
}

fn compile_binop(k: BinOpKind, l: &Node, r: &Node, ctxt: &mut Ctxt) -> ll::ValueId {
    use BinOpKind::*;

    let l = ctxt.nodes[l];
    let r = ctxt.nodes[r];

    if matches!(k, Plus | Minus | Mul | Div | Mod | Pow) {
        let lerr = tag_err(l, Tag::NUM, ctxt);
        let rerr = tag_err(r, Tag::NUM, ctxt);

        let err = ctxt.push_compute(ll::Expr::Or(lerr, rerr));
        err_chk(err, "trying to calculate with non-nums!", ctxt);

        let l = extract_num(l, ctxt);
        let r = extract_num(r, ctxt);

        let x = match k {
            BinOpKind::Plus => ctxt.push_compute(ll::Expr::NumOp(ll::NumOpKind::Plus, ll::NumKind::Float, l, r)),
            BinOpKind::Minus => ctxt.push_compute(ll::Expr::NumOp(ll::NumOpKind::Minus, ll::NumKind::Float, l, r)),
            BinOpKind::Mul => ctxt.push_compute(ll::Expr::NumOp(ll::NumOpKind::Mul, ll::NumKind::Float, l, r)),
            BinOpKind::Div => ctxt.push_compute(ll::Expr::NumOp(ll::NumOpKind::Div, ll::NumKind::Float, l, r)),
            BinOpKind::Mod => ctxt.push_compute(ll::Expr::NumOp(ll::NumOpKind::Mod, ll::NumKind::Float, l, r)),
            BinOpKind::Pow => call_extra_fn("pow", &[l, r], ctxt),
            _ => unreachable!(),
        };

        mk_num(x, ctxt)
    } else if matches!(k, IsEqual | IsNotEqual) {
        let l = alloc_val(l, ctxt);
        let r = alloc_val(r, ctxt);

        let mut b /* i1 */ = call_extra_fn("eq", &[l, r], ctxt);
        if matches!(k, IsNotEqual) {
            b = ctxt.push_compute(ll::Expr::Not(b));
        }

        mk_bool(b, ctxt)
    } else if matches!(k, Concat) {
        let l = alloc_val(l, ctxt);
        let r = alloc_val(r, ctxt);
        let out = alloc(ctxt);

        call_extra_fn("concat", &[l, r, out], ctxt);

        load_val(out, ctxt)
    } else if matches!(k, Lt | Le | Gt | Ge) {
        let lerr = tag_err(l, Tag::NUM, ctxt);
        let rerr = tag_err(r, Tag::NUM, ctxt);

        let err = ctxt.push_compute(ll::Expr::Or(lerr, rerr));
        err_chk(err, "trying to compare non-nums!", ctxt);

        let l = extract_num(l, ctxt);
        let r = extract_num(r, ctxt);

        let kind = match k {
            BinOpKind::Lt => ll::NumOpKind::Lt,
            BinOpKind::Le => ll::NumOpKind::Le,
            BinOpKind::Gt => ll::NumOpKind::Gt,
            BinOpKind::Ge => ll::NumOpKind::Ge,
            _ => unreachable!(),
        };

        let x = ctxt.push_compute(ll::Expr::NumOp(kind, ll::NumKind::Float, l, r));

        mk_bool(x, ctxt)
    } else { unreachable!() }
}
