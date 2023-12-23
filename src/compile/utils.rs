use super::*;

// error handling

pub fn err_chk(err_cond: ll::ValueId, error_msg: &str, ctxt: &mut Ctxt) {
    let errblock = ctxt.b.alloc_block();
    let goodblock = ctxt.b.alloc_block();

    ctxt.b.push_cond_br(err_cond, errblock, goodblock);

    ctxt.b.set_active_block(errblock);

    let msg = ctxt.b.alloc_string(error_msg);
    let str_t = ctxt.str_t();
    let msg = ctxt.b.push_bit_cast(msg, str_t);
    call_extra_fn("puts", &[msg], ctxt);

    let i32_t = ctxt.i32_t();
    let one = ctxt.b.push_const_int(i32_t, 1);
    call_extra_fn("exit", &[one], ctxt);

    ctxt.b.push_unreachable();

    ctxt.b.set_active_block(goodblock);
}

// returns `true` if err was found.
pub fn tag_err(v: ll::ValueId, tag: Tag, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let tag = ctxt.b.push_const_int(i64_t, tag as _);
    let vtag = ctxt.b.push_extract_value(v, 0);
    ctxt.b.push_i_is_not_equal(vtag, tag)
}

pub fn mk_nil(ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let tag = ctxt.b.push_const_int(i64_t, Tag::NIL as _);

    let value_t = ctxt.value_t();
    let a = ctxt.b.push_poison(value_t);
    let a = ctxt.b.push_insert_value(a, tag, 0);

    a
}

pub fn mk_num(x: ll::ValueId /* f64 */, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let x = ctxt.b.push_bit_cast(x, i64_t);
    let i64_t = ctxt.i64_t();
    let tag = ctxt.b.push_const_int(i64_t, Tag::NUM as _);

    let value_t = ctxt.value_t();
    let a = ctxt.b.push_poison(value_t);
    let a = ctxt.b.push_insert_value(a, tag, 0);
    let a = ctxt.b.push_insert_value(a, x, 1);

    a
}

pub fn mk_str(s: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let tag = ctxt.b.push_const_int(i64_t, Tag::STR as _);

    let i64_t = ctxt.i64_t();
    let s = ctxt.b.push_ptr_to_int(s, i64_t);

    let value_t = ctxt.value_t();
    let a = ctxt.b.push_poison(value_t);
    let a = ctxt.b.push_insert_value(a, tag, 0);
    let a = ctxt.b.push_insert_value(a, s, 1);

    a
}

pub fn mk_bool(x: ll::ValueId /* i1 */, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let x = ctxt.b.push_zext(x, i64_t);
    let i64_t = ctxt.i64_t();
    let tag = ctxt.b.push_const_int(i64_t, Tag::BOOL as _);

    let value_t = ctxt.value_t();
    let a = ctxt.b.push_poison(value_t);
    let a = ctxt.b.push_insert_value(a, tag, 0);
    let a = ctxt.b.push_insert_value(a, x, 1);

    a
}

pub fn extract_num(x: ll::ValueId /* Value */, ctxt: &mut Ctxt) -> ll::ValueId {
    let x = ctxt.b.push_extract_value(x, 1);
    let f64_t = ctxt.f64_t();
    let x = ctxt.b.push_bit_cast(x, f64_t);

    x
}

// f should be generated using LLVMAddFunction of type v2void_t.
pub fn mk_fn(f: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let i64_t = ctxt.i64_t();
    let val = ctxt.b.push_ptr_to_int(f, i64_t);

    let i64_t = ctxt.i64_t();
    let tag = ctxt.b.push_const_int(i64_t, Tag::FN as _);

    let value_t = ctxt.value_t();
    let a = ctxt.b.push_poison(value_t);
    let a = ctxt.b.push_insert_value(a, tag, 0);
    let a = ctxt.b.push_insert_value(a, val, 1);

    a
}
