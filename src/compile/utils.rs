use super::*;

// error handling

pub fn err_chk(err_cond: ll::ValueId, error_msg: &str, ctxt: &mut Ctxt) {
    let errblock = ctxt.b.alloc_block();
    let goodblock = ctxt.b.alloc_block();

    ctxt.b.push_cond_br(err_cond, errblock, goodblock);

    ctxt.b.set_active_block(errblock);

    let msg = ctxt.b.alloc_string(error_msg);
    let msg = ctxt.b.push_bit_cast(msg, ctxt.str_t());
    call_extra_fn("puts", &[msg], ctxt);

    let one = ctxt.b.push_const_int(ctxt.i32_t(), 1);
    call_extra_fn("exit", &[one], ctxt);

    ctxt.b.push_unreachable();

    ctxt.b.set_active_block(goodblock);
}

// returns `true` if err was found.
pub fn tag_err(v: ll::ValueId, tag: Tag, ctxt: &mut Ctxt) -> ll::ValueId {
    let tag = ctxt.b.push_const_int(ctxt.i64_t(), tag as _);
    let vtag = ctxt.b.push_extract_value(v, 0);
    ctxt.b.push_i_is_not_equal(vtag, tag)
}

// mk_* functions:
// construct a 'Value' using tag and payload.
pub fn mk_val(payload: ll::ValueId, tag: Tag, ctxt: &mut Ctxt) -> ll::ValueId {
    let tag = ctxt.b.push_const_int(ctxt.i64_t(), tag as _);

    let a = ctxt.b.push_poison(ctxt.value_t());
    let a = ctxt.b.push_insert_value(a, tag, 0);
    let a = ctxt.b.push_insert_value(a, payload, 1);

    a
}

pub fn mk_nil(ctxt: &mut Ctxt) -> ll::ValueId {
    let payload = ctxt.b.push_poison(ctxt.i64_t());
    mk_val(payload, Tag::NIL, ctxt)
}

pub fn mk_num(x: ll::ValueId /* f64 */, ctxt: &mut Ctxt) -> ll::ValueId {
    let x = ctxt.b.push_bit_cast(x, ctxt.i64_t());
    mk_val(x, Tag::NUM, ctxt)
}

pub fn mk_str(s: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let s = ctxt.b.push_ptr_to_int(s, ctxt.i64_t());
    mk_val(s, Tag::STR, ctxt)
}

pub fn mk_bool(x: ll::ValueId /* i1 */, ctxt: &mut Ctxt) -> ll::ValueId {
    let x = ctxt.b.push_zext(x, ctxt.i64_t());
    mk_val(x, Tag::BOOL, ctxt)
}

pub fn mk_table(x: ll::ValueId /* table* */, ctxt: &mut Ctxt) -> ll::ValueId {
    let x = ctxt.b.push_ptr_to_int(x, ctxt.i64_t());
    mk_val(x, Tag::TABLE_PTR, ctxt)
}

pub fn mk_fn(f: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let val = ctxt.b.push_ptr_to_int(f, ctxt.i64_t());
    mk_val(val, Tag::FN, ctxt)
}

// extracting the payload from a value:
pub fn extract_num(x: ll::ValueId /* Value */, ctxt: &mut Ctxt) -> ll::ValueId {
    let x = ctxt.b.push_extract_value(x, 1);
    let x = ctxt.b.push_bit_cast(x, ctxt.f64_t());

    x
}

pub fn extract_bool(x: ll::ValueId /* Value */, ctxt: &mut Ctxt) -> ll::ValueId /* i1 */ {
    let val = ctxt.b.push_extract_value(x, 1);
    let one = ctxt.b.push_const_int(ctxt.i64_t(), 1);
    let ret = ctxt.b.push_i_is_equal(val, one);

    ret
}

