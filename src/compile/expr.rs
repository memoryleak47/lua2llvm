use super::*;

fn compile_intrinsic(intrinsic: &Intrinsic, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        match intrinsic {
            Intrinsic::Print(var) => {
                let t = alloc_val(ctxt.nodes[var], ctxt);
                call_extra_fn("print", &[t], ctxt);

                mk_nil(ctxt)
            },
            Intrinsic::Throw(s) => {
                let s = format!("{}\0", s);
                let s = LLVMBuildGlobalString(ctxt.builder, s.as_ptr() as *const _, EMPTY);
                call_extra_fn("throw_", &[s], ctxt);

                mk_nil(ctxt)
            },
            Intrinsic::Type(v) => {
                let v = alloc_val(ctxt.nodes[v], ctxt);
                let t = alloc(ctxt);
                call_extra_fn("type", &[v, t], ctxt);

                load_val(t, ctxt)
            },
            Intrinsic::Next(v1, v2) => {
                let v1 = alloc_val(ctxt.nodes[v1], ctxt);
                let v2 = alloc_val(ctxt.nodes[v2], ctxt);
                let t = alloc(ctxt);
                call_extra_fn("next", &[v1, v2, t], ctxt);

                load_val(t, ctxt)
            },
        }
    }
}

pub fn compile_expr(e: &Expr, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        match e {
            Expr::Nil => mk_nil(ctxt),
            Expr::Bool(b) => {
                let b = LLVMConstInt(ctxt.bool_t(), *b as _, 0);

                mk_bool(b, ctxt)
            }
            Expr::Num(x) => {
                let x = LLVMConstReal(ctxt.f64_t(), *x);

                mk_num(x, ctxt)
            },
            Expr::Str(s) => {
                let s = format!("{}\0", s);
                let s = LLVMBuildGlobalString(ctxt.builder, s.as_ptr() as *const _, EMPTY);

                mk_str(s, ctxt)
            },
            Expr::NewTable => {
                let var = alloc(ctxt);
                call_extra_fn("new_table", &[var], ctxt);

                load_val(var, ctxt)
            }
            Expr::LitFunction(fid) => {
                mk_fn(ctxt.lit_fns[fid], ctxt)
            },
            Expr::Index(t, i) => {
                let t = alloc_val(ctxt.nodes[t], ctxt);
                let i = alloc_val(ctxt.nodes[i], ctxt);
                let out = alloc(ctxt);
                call_extra_fn("table_get", &[t, i, out], ctxt);
                load_val(out, ctxt)
            },
            Expr::Arg => {
                let fid = ctxt.current_fid;
                let param = LLVMGetParam(ctxt.lit_fns[&fid], 0);
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
            Expr::Intrinsic(intrinsic) => compile_intrinsic(intrinsic, ctxt),
        }
    }
}

fn compile_binop(k: BinOpKind, l: &Node, r: &Node, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        use BinOpKind::*;

        let l = ctxt.nodes[l];
        let r = ctxt.nodes[r];

        if matches!(k, Plus | Minus | Mul | Div | Mod | Pow) {
            let lerr = tag_err(l, Tag::NUM, ctxt);
            let rerr = tag_err(r, Tag::NUM, ctxt);

            let err = LLVMBuildOr(ctxt.builder, lerr, rerr, EMPTY);
            err_chk(err, "trying to calculate with non-nums!", ctxt);

            let l = extract_num(l, ctxt);
            let r = extract_num(r, ctxt);

            let x = match k {
                BinOpKind::Plus => LLVMBuildFAdd(ctxt.builder, l, r, EMPTY),
                BinOpKind::Minus => LLVMBuildFSub(ctxt.builder, l, r, EMPTY),
                BinOpKind::Mul => LLVMBuildFMul(ctxt.builder, l, r, EMPTY),
                BinOpKind::Div => LLVMBuildFDiv(ctxt.builder, l, r, EMPTY),
                BinOpKind::Mod => LLVMBuildFRem(ctxt.builder, l, r, EMPTY),
                BinOpKind::Pow => call_extra_fn("pow", &[l, r], ctxt),
                _ => unreachable!(),
            };

            mk_num(x, ctxt)
        } else if matches!(k, IsEqual | IsNotEqual) {
            let l = alloc_val(l, ctxt);
            let r = alloc_val(r, ctxt);

            let mut b /* LLVM i1 */ = call_extra_fn("eq", &[l, r], ctxt);
            if matches!(k, IsNotEqual) {
                let one = LLVMConstInt(ctxt.bool_t(), 1, 0);
                b = LLVMBuildXor(ctxt.builder, b, one, EMPTY);
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

            let err = LLVMBuildOr(ctxt.builder, lerr, rerr, EMPTY);
            err_chk(err, "trying to compare non-nums!", ctxt);

            let l = extract_num(l, ctxt);
            let r = extract_num(r, ctxt);

            let pred = match k {
                BinOpKind::Lt => LLVMRealPredicate::LLVMRealOLT,
                BinOpKind::Le => LLVMRealPredicate::LLVMRealOLE,
                BinOpKind::Gt => LLVMRealPredicate::LLVMRealOGT,
                BinOpKind::Ge => LLVMRealPredicate::LLVMRealOGE,
                _ => unreachable!(),
            };

            let x = LLVMBuildFCmp(ctxt.builder, pred, l, r, EMPTY);

            mk_bool(x, ctxt)
        } else { unreachable!() }
    }
}
