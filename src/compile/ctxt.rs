use crate::ir::*;

use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;
use llvm::target_machine::LLVMGetDefaultTargetTriple;


#[derive(Clone)]
pub struct ExtraFn {
    pub f: LLVMValueRef,
    pub ftype: LLVMTypeRef,
}

pub struct Ctxt {
    pub llctxt: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,

    pub nodes: HashMap<Node, LLVMValueRef>,

    // functions defined in extra/
    pub extra_fns: HashMap<String, ExtraFn>,

    pub lit_fns: HashMap<FnId, LLVMValueRef>,

    pub start_fn: LLVMValueRef,

    pub current_fid: FnId,

    // if we encounter a `break`, go to this block.
    pub break_bb: Option<LLVMBasicBlockRef>,
}

impl Ctxt {
    pub fn new() -> Self {
        unsafe {
            let llctxt = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(b"luamod\0".as_ptr() as *const _, llctxt);
            let target = LLVMGetDefaultTargetTriple();
            LLVMSetTarget(module, target);

            let builder = LLVMCreateBuilderInContext(llctxt);

            let start_fn = {
                let mut args = [];
                let start_function_type = LLVMFunctionType(LLVMVoidType(), args.as_mut_ptr(), args.len() as _, 0);

                LLVMAddFunction(module, b"main\0".as_ptr() as *const _, start_function_type)
            };

            let break_bb = None;

            Ctxt {
                llctxt,
                module,
                builder,
                nodes: Default::default(),
                extra_fns: Default::default(),
                start_fn,
                lit_fns: Default::default(),
                current_fid: 0,
                break_bb,
            }
        }
    }

    pub fn i32_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(self.llctxt) }
    }

    pub fn i64_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMInt64TypeInContext(self.llctxt) }
    }

    pub fn void_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMVoidType() }
    }

    pub fn f64_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMDoubleTypeInContext(self.llctxt) }
    }

    pub fn value_t(&mut self) -> LLVMTypeRef {
        let mut elements = [self.i32_t(), self.i32_t(), self.i64_t()];
        unsafe { LLVMStructTypeInContext(self.llctxt, elements.as_mut_ptr(), elements.len() as u32, 0) }
    }

    pub fn value_ptr_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.value_t(), 0) }
    }

    pub fn v2v_t(&mut self) -> LLVMTypeRef {
        let mut args = [self.value_ptr_t(), self.i32_t(), self.value_ptr_t()];
        unsafe { LLVMFunctionType(self.void_t(), args.as_mut_ptr(), args.len() as _, 0) }
    }

    pub fn v2v_ptr_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.v2v_t(), 0) }
    }

    pub fn i8_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMInt8TypeInContext(self.llctxt) }
    }

    pub fn bool_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMInt1TypeInContext(self.llctxt) }
    }

    pub fn str_t(&mut self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.i8_t(), 0) }
    }
}

impl Drop for Ctxt {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.llctxt);
        }
    }
}
