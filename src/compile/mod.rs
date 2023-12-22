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

    let start_function_type = ll::Type::Function(Box::new(ll::Type::Void), vec![]);
    ctxt.start_fn = ctxt.alloc_fn("main".to_string(), start_function_type);
    let i64_t = ctxt.i64_t();
    ctxt.m.structs.insert(ll::StructId(0), vec![i64_t.clone(), i64_t]);

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
        let v2void_t = ctxt.v2void_t();
        let function = ctxt.alloc_fn(name, v2void_t);
        ctxt.lit_fns.insert(fid, function);
    }

    compile_start_fn(ctxt.ir.main_fn, &mut ctxt);

    // compile lit fns

    for &fid in &fids {
        compile_fn(fid, &mut ctxt);
    }

    ll::dump(ctxt.m);
}

// will allocate a variable of type Value.
fn alloc(ctxt: &mut Ctxt) -> ll::ValueId {
    let ty = ctxt.value_t();
    let f = ctxt.current_fn_impl();
    let n = ll::VarId(f.vars.len());
    f.vars.insert(n, ty);

    ctxt.push_compute(ll::Expr::Var(n))
}

// will allocate a variable pointing to the Value `x`.
fn alloc_val(x: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let v = alloc(ctxt);
    ctxt.push_st(ll::Statement::PtrStore(x, v));

    v
}

// will load a Value*.
fn load_val(x: ll::ValueId, ctxt: &mut Ctxt) -> ll::ValueId {
    let value_t = ctxt.value_t();
    ctxt.push_compute(ll::Expr::PtrLoad(value_t, x))
}
