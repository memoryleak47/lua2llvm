use super::*;

pub fn compile_expr(e: &Expr, current_fn: FnId, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        match e {
            Expr::Nil => mk_nil(ctxt),
            Expr::Num(x) => {
                let x = LLVMConstReal(ctxt.f64_t(), *x);

                mk_num(x, ctxt)
            },
            Expr::NewTable => {
                let var = alloc(ctxt);
                call_extra_fn("new_table", &[var], ctxt);

                load_val(var, ctxt)
            }
            Expr::NativeFn(i) => mk_natfn(*i, ctxt),
            Expr::LitFunction(fid, upnodes) => {
                let mut opt /*: Option<LLVM i32> */ = None;
                for n in upnodes {
                    let v = alloc_val(ctxt.nodes[n], ctxt);
                    let o = call_extra_fn("uvstack_push", &[v], ctxt);
                    if opt.is_none() { opt = Some(o); }
                }
                let i = opt.unwrap_or_else(|| LLVMConstInt(ctxt.i32_t(), 0, 0));
                mk_fn(ctxt.lit_fns[fid], i, ctxt)
            },
            Expr::Index(t, i) => {
                let t = alloc_val(ctxt.nodes[t], ctxt);
                let i = alloc_val(ctxt.nodes[i], ctxt);
                let out = alloc(ctxt);
                call_extra_fn("table_get", &[t, i, out], ctxt);
                load_val(out, ctxt)
            },
            Expr::FnCall(f, arg) => {
                let f = ctxt.nodes[f];
                let arg = ctxt.nodes[arg];

                fn_call(f, arg, ctxt)
            }
            Expr::Arg => {
                let param = LLVMGetParam(ctxt.lit_fns[&current_fn], 0);
                load_val(param, ctxt)
            },
            Expr::Upvalue(i) => {
                let base /* LLVM i32 */ = LLVMGetParam(ctxt.lit_fns[&current_fn], 1);
                let offset /* LLVM i32 */  = LLVMConstInt(ctxt.i32_t(), *i as _, 0);
                let idx /* LLVM i32 */ = LLVMBuildAdd(ctxt.builder, base, offset, EMPTY);

                let var = alloc(ctxt);
                call_extra_fn("uvstack_get", &[idx, var], ctxt);

                load_val(var, ctxt)
            },
            Expr::BinOp(k, l, r) => compile_binop(*k, l, r, ctxt),
            x => {
                println!("ignoring other Expr {:?}!", x);

                mk_nil(ctxt)
            },
        }
    }
}

fn compile_binop(k: BinOpKind, l: &Node, r: &Node, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        let l = ctxt.nodes[l];
        let r = ctxt.nodes[r];

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
            _ => todo!(),
        };

        mk_num(x, ctxt)
    }
}

fn fn_call(f_val: LLVMValueRef /* Value with FN tag */, arg: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value */ {
    unsafe {
        // check tag
        let t = tag_err(f_val, Tag::FN, ctxt);
        err_chk(t, "trying to call non-function!", ctxt);

        // call fn
        let uvstack_index /* i32 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 1, EMPTY);
        let f /* i64 */ = LLVMBuildExtractValue(ctxt.builder, f_val, 2, EMPTY);
        let f /* Value (*fn)(Value) */ = LLVMBuildIntToPtr(ctxt.builder, f, ctxt.v2v_ptr_t(), EMPTY);

        let out = alloc(ctxt);
        let mut fargs = [alloc_val(arg, ctxt), uvstack_index, out];
        LLVMBuildCall2(
            /*builder: */ ctxt.builder,
            /*type: */ ctxt.v2v_t(),
            /*Fn: */ f,
            /*Args: */ fargs.as_mut_ptr(),
            /*Num Args: */ fargs.len() as u32,
            /*Name: */ EMPTY,
        );

        load_val(out, ctxt)
    }
}
