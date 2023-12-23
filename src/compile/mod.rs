use std::collections::HashMap;

use crate::ir::*;
use crate::infer::Infer;
use crate::layout::Layout;
use crate::ll;

mod ctxt;
use ctxt::*;

mod extra;
use extra::*;

mod expr;
use expr::*;

mod start;
use start::*;

mod func;
use func::*;

mod utils;
use utils::*;

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub enum Tag {
    TABLE_PTR = 0,
    FN = 1,
    NIL = 2,
    NUM = 3,
    STR = 4,
    BOOL = 5,
}

pub fn compile(ir: &IR, inf: &Infer, layout: &Layout) {
    let mut ctxt = Ctxt::new(ir, inf, layout);

    ctxt.value_struct_id = Some(ctxt.b.alloc_struct(vec![ctxt.i64_t(), ctxt.i64_t()]));

    // table implementation:
    declare_extra_fn("new_table", ctxt.void_t(), &[ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("table_set", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("table_get", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);

    // ops:
    declare_extra_fn("eq", ctxt.bool_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("concat", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("pow", ctxt.f64_t(), &[ctxt.f64_t(), ctxt.f64_t()], &mut ctxt);
    declare_extra_fn("len", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);

    // error handling
    declare_extra_fn("puts", ctxt.void_t(), &[ctxt.str_t()], &mut ctxt);
    declare_extra_fn("exit", ctxt.void_t(), &[ctxt.i32_t()], &mut ctxt);

    // intrinsics
    declare_extra_fn("next", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("print", ctxt.void_t(), &[ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("type", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
    declare_extra_fn("throw_", ctxt.void_t(), &[ctxt.str_t()], &mut ctxt);

    // declare lit fns
    let mut fids: Vec<_> = ctxt.ir.fns.keys().cloned().collect();
    fids.sort();

    for &fid in &fids {
        let name = format!("f{}", fid);
        let function = ctxt.b.alloc_fn(name, ctxt.v2void_t());
        ctxt.lit_fns.insert(fid, function);
    }

    compile_start_fn(ctxt.ir.main_fn, &mut ctxt);

    // compile lit fns

    for &fid in &fids {
        compile_fn(fid, &mut ctxt);
    }

    ll::dump(ctxt.b.finish());
}

// will allocate a variable of type Value.
fn alloc(ctxt: &mut Ctxt) -> ll::ValueId {
    let var_id = ctxt.b.alloc_var(ctxt.value_t());

    ctxt.b.push_var(var_id)
}

// will allocate a variable pointing to the Value `x`.
fn alloc_val(x: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let v = alloc(ctxt);
    ctxt.b.push_ptr_store(x, v);

    v
}

// will load a Value*.
fn load_val(x: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    ctxt.b.push_ptr_load(ctxt.value_t(), x)
}
