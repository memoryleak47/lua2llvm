use crate::ir::*;
use crate::infer::*;
use crate::layout::*;

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

use llvm::core::*;
use llvm::prelude::*;
use llvm::{LLVMIntPredicate, LLVMRealPredicate};

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

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

// TODO use the layout.
pub fn compile(ir: &IR, inf: &Infer, layout: &Layout) {
    unsafe {
        let mut ctxt = Ctxt::new();

        // table implementation:
        declare_extra_fn("new_table", ctxt.void_t(), &[ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("table_set", ctxt.void_t(), &[ctxt.value_ptr_t(); 3], &mut ctxt);
        declare_extra_fn("table_get", ctxt.void_t(), &[ctxt.value_ptr_t(); 3], &mut ctxt);

        // ops:
        declare_extra_fn("eq", ctxt.bool_t(), &[ctxt.value_ptr_t(); 2], &mut ctxt);
        declare_extra_fn("concat", ctxt.void_t(), &[ctxt.value_ptr_t(); 3], &mut ctxt);
        declare_extra_fn("pow", ctxt.f64_t(), &[ctxt.f64_t(); 2], &mut ctxt);
        declare_extra_fn("len", ctxt.void_t(), &[ctxt.value_ptr_t(); 2], &mut ctxt);

        // error handling
        declare_extra_fn("puts", ctxt.void_t(), &[ctxt.str_t()], &mut ctxt);
        declare_extra_fn("exit", ctxt.void_t(), &[ctxt.i32_t()], &mut ctxt);

        // intrinsics
        declare_extra_fn("next", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("print", ctxt.void_t(), &[ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("type", ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("throw_", ctxt.void_t(), &[ctxt.str_t()], &mut ctxt);

        // declare lit fns
        for &fid in ir.fns.keys() {
            let name = format!("f{}\0", fid);
            let function = LLVMAddFunction(ctxt.module, name.as_bytes().as_ptr() as *const _, ctxt.v2void_t());
            ctxt.lit_fns.insert(fid, function);
        }

        compile_start_fn(ir.main_fn, &mut ctxt);

        // compile lit fns
        for &fid in ir.fns.keys() {
            compile_fn(ctxt.lit_fns[&fid], fid, ir, &mut ctxt);
        }

        LLVMDumpModule(ctxt.module);
    }
}

// will allocate a variable of type Value.
unsafe fn alloc(ctxt: &mut Ctxt) -> LLVMValueRef /* Value* */ {
    let builder = LLVMCreateBuilderInContext(ctxt.llctxt);
    LLVMPositionBuilderBefore(builder, ctxt.alloca_br_instr.unwrap());
    let ret = LLVMBuildAlloca(builder, ctxt.value_t(), EMPTY);
    LLVMDisposeBuilder(builder);

    ret
}

// will allocate a variable pointing to the Value `x`.
unsafe fn alloc_val(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value* */ {
    let var = alloc(ctxt);
    LLVMBuildStore(ctxt.builder, x, var);

    var
}

// will load a Value*.
unsafe fn load_val(x: LLVMValueRef /* Value* */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value */ {
    LLVMBuildLoad2(ctxt.builder, ctxt.value_t(), x, EMPTY)
}
