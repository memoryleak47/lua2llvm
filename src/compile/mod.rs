use crate::ir::*;

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
use llvm::LLVMIntPredicate;

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

pub fn compile(ir: &IR) {
    unsafe {
        let mut ctxt = Ctxt::new();

        // table implementation:
        declare_extra_fn("new_table", ctxt.void_t(), &[ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("table_set", ctxt.void_t(), &[ctxt.value_ptr_t(); 3], &mut ctxt);
        declare_extra_fn("table_get", ctxt.void_t(), &[ctxt.value_ptr_t(); 3], &mut ctxt);

        // upvalue implementation:
        declare_extra_fn("uvstack_push", ctxt.i32_t(), &[ctxt.value_ptr_t()], &mut ctxt);
        declare_extra_fn("uvstack_get", ctxt.void_t(), &[ctxt.i32_t(), ctxt.value_ptr_t()], &mut ctxt);

        declare_extra_fn("eq", ctxt.bool_t(), &[ctxt.value_ptr_t(); 2], &mut ctxt);

        // error handling
        declare_extra_fn("puts", ctxt.void_t(), &[ctxt.str_t()], &mut ctxt);
        declare_extra_fn("exit", ctxt.void_t(), &[ctxt.i32_t()], &mut ctxt);

        // native functions:
        for fname in NATIVE_FNS {
            declare_extra_fn(fname, ctxt.void_t(), &[ctxt.value_ptr_t(), ctxt.i32_t(), ctxt.value_ptr_t()], &mut ctxt);
        }

        // declare lit fns
        for fid in 0..ir.fns.len() {
            let name = format!("f{}\0", fid);
            let function = LLVMAddFunction(ctxt.module, name.as_bytes().as_ptr() as *const _, ctxt.v2v_t());
            ctxt.lit_fns.insert(fid, function);
        }

        compile_start_fn(ir.main_fn, &mut ctxt);

        // compile lit fns
        for fid in 0..ir.fns.len() {
            compile_fn(ctxt.lit_fns[&fid], fid, ir, &mut ctxt);
        }

        LLVMDumpModule(ctxt.module);
    }
}

// will allocate a variable of type Value.
fn alloc(ctxt: &mut Ctxt) -> LLVMValueRef /* Value* */ {
    unsafe {
        LLVMBuildAlloca(ctxt.builder, ctxt.value_t(), EMPTY)
    }
}

// will allocate a variable pointing to the Value `x`.
fn alloc_val(x: LLVMValueRef /* Value */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value* */ {
    unsafe {
        let var = alloc(ctxt);
        LLVMBuildStore(ctxt.builder, x, var);

        var
    }
}

// will load a Value*.
fn load_val(x: LLVMValueRef /* Value* */, ctxt: &mut Ctxt) -> LLVMValueRef /* Value */ {
    unsafe {
        LLVMBuildLoad2(ctxt.builder, ctxt.value_t(), x, EMPTY)
    }
}
