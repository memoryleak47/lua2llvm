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

    // the instruction branching from the alloca-block to the main-block.
    pub alloca_br_instr: Option<LLVMValueRef>,
}

impl Ctxt {
    pub unsafe fn new() -> Self {
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

        Ctxt {
            llctxt,
            module,
            builder,
            nodes: Default::default(),
            extra_fns: Default::default(),
            start_fn,
            lit_fns: Default::default(),
            current_fid: 0,
            break_bb: None,
            alloca_br_instr: None,
        }
    }

    pub unsafe fn i32_t(&mut self) -> LLVMTypeRef {
        LLVMInt32TypeInContext(self.llctxt)
    }

    pub unsafe fn i64_t(&mut self) -> LLVMTypeRef {
        LLVMInt64TypeInContext(self.llctxt)
    }

    pub unsafe fn void_t(&mut self) -> LLVMTypeRef {
        LLVMVoidType()
    }

    pub unsafe fn f64_t(&mut self) -> LLVMTypeRef {
        LLVMDoubleTypeInContext(self.llctxt)
    }

    pub unsafe fn value_t(&mut self) -> LLVMTypeRef {
        let mut elements = [self.i64_t(), self.i64_t()];
        LLVMStructTypeInContext(self.llctxt, elements.as_mut_ptr(), elements.len() as u32, 0)
    }

    pub unsafe fn value_ptr_t(&mut self) -> LLVMTypeRef {
        LLVMPointerType(self.value_t(), 0)
    }

    pub unsafe fn v2void_t(&mut self) -> LLVMTypeRef {
        let mut args = [self.value_ptr_t()];
        LLVMFunctionType(self.void_t(), args.as_mut_ptr(), args.len() as _, 0)
    }

    pub unsafe fn v2void_ptr_t(&mut self) -> LLVMTypeRef {
        LLVMPointerType(self.v2void_t(), 0)
    }

    pub unsafe fn i8_t(&mut self) -> LLVMTypeRef {
        LLVMInt8TypeInContext(self.llctxt)
    }

    pub unsafe fn bool_t(&mut self) -> LLVMTypeRef {
        LLVMInt1TypeInContext(self.llctxt)
    }

    pub unsafe fn str_t(&mut self) -> LLVMTypeRef {
        LLVMPointerType(self.i8_t(), 0)
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
