use super::*;

// error handling

pub fn err_chk(err_cond: ll::ValueId, error_msg: &str, ctxt: &mut Ctxt) {
    let errblock = ctxt.alloc_block();
    let goodblock = ctxt.alloc_block();

    ctxt.push_st(ll::Statement::CondBr(err_cond, errblock, goodblock));

    ctxt.current_ll_bid = errblock;

    let def = ll::GlobalDef::String(error_msg.to_string());
    let gid = ctxt.alloc_global(def);
    let val = ll::ValueId::Global(gid);
    let str_t = ctxt.str_t();
    let val = ctxt.push_compute(ll::Expr::BitCast(val, str_t));
    call_extra_fn("puts", &[val], ctxt);

    let i32_t = ctxt.i32_t();
    let one = ctxt.push_compute(ll::Expr::ConstInt(i32_t, 1));
    call_extra_fn("exit", &[one], ctxt);

    ctxt.push_st(ll::Statement::Unreachable);

    ctxt.current_ll_bid = goodblock;
}

// returns `true` if err was found.
pub fn tag_err(v: ll::ValueId, tag: Tag, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let tag = ctxt.push_compute(ll::Expr::ConstInt(i64_t, tag as _));
    let vtag = ctxt.push_compute(ll::Expr::ExtractValue(v, 0));
    ctxt.push_compute(ll::Expr::NumOp(ll::NumOpKind::IsNotEqual, ll::NumKind::Int, vtag, tag))
}

pub fn mk_nil(ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let tag = ctxt.push_compute(ll::Expr::ConstInt(i64_t, Tag::NIL as _));

    let value_t = ctxt.value_t();
    let a = ctxt.push_compute(ll::Expr::Poison(value_t));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, tag, 0));

    a
}

pub fn mk_num(x: ll::ValueId /* f64 */, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let x = ctxt.push_compute(ll::Expr::BitCast(x, i64_t));
    let i64_t = ctxt.i64_t();
    let tag = ctxt.push_compute(ll::Expr::ConstInt(i64_t, Tag::NUM as _));

    let value_t = ctxt.value_t();
    let a = ctxt.push_compute(ll::Expr::Poison(value_t));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, tag, 0));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, x, 1));

    a
}

pub fn mk_str(s: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let tag = ctxt.push_compute(ll::Expr::ConstInt(i64_t, Tag::STR as _));

    let i64_t = ctxt.i64_t();
    let s = ctxt.push_compute(ll::Expr::PtrToInt(s, i64_t));

    let value_t = ctxt.value_t();
    let a = ctxt.push_compute(ll::Expr::Poison(value_t));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, tag, 0));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, s, 1));

    a
}

pub fn mk_bool(x: ll::ValueId /* i1 */, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let x = ctxt.push_compute(ll::Expr::ZExt(x, i64_t));
    let i64_t = ctxt.i64_t();
    let tag = ctxt.push_compute(ll::Expr::ConstInt(i64_t, Tag::BOOL as _));

    let value_t = ctxt.value_t();
    let a = ctxt.push_compute(ll::Expr::Poison(value_t));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, tag, 0));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, x, 1));

    a
}

pub fn extract_num(x: ll::ValueId /* Value */, ctxt: &mut Ctxt) -> ll::ValueId {
    let x = ctxt.push_compute(ll::Expr::ExtractValue(x, 1));
    let f64_t = ctxt.f64_t();
    let x = ctxt.push_compute(ll::Expr::BitCast(x, f64_t));

    x
}

// f should be generated using LLVMAddFunction of type v2void_t.
pub fn mk_fn(f: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let val = ctxt.push_compute(ll::Expr::PtrToInt(f, i64_t));

    let i64_t = ctxt.i64_t();
    let tag = ctxt.push_compute(ll::Expr::ConstInt(i64_t, Tag::FN as _));

    let value_t = ctxt.value_t();
    let a = ctxt.push_compute(ll::Expr::Poison(value_t));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, tag, 0));
    let a = ctxt.push_compute(ll::Expr::InsertValue(a, val, 1));

    a
}
