use std::collections::HashMap;

use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::target_machine::LLVMGetDefaultTargetTriple;

use crate::ll::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

pub fn dump(m: Module) {
    unsafe {
        let llvm_ctxt = LLVMContextCreate();
        let builder = LLVMCreateBuilderInContext(llvm_ctxt);
        let global_map = Default::default();

        let llvm_mod = with_string("luamod", |s| LLVMModuleCreateWithNameInContext(s, llvm_ctxt));
        let target = LLVMGetDefaultTargetTriple();
        LLVMSetTarget(llvm_mod, target);

        let mut ctxt = Ctxt {
            llvm_ctxt,
            builder,
            global_map,
            m,
            llvm_mod,
        };

        translate(&mut ctxt);
        LLVMDumpModule(ctxt.llvm_mod);

        LLVMDisposeModule(ctxt.llvm_mod);
        LLVMContextDispose(ctxt.llvm_ctxt);
    }
}

struct Ctxt {
    llvm_ctxt: LLVMContextRef,
    builder: LLVMBuilderRef,
    global_map: HashMap<GlobalValueId, LLVMValueRef>,
    m: Module,
    llvm_mod: LLVMModuleRef,
}

unsafe fn translate(ctxt: &mut Ctxt) {
    unsafe {
        // declare all functions. String globals are declared on demand.
        for (gid, d) in ctxt.m.global_defs.clone() {
            let GlobalDef::Function(name, ty, _imp) = d else { continue; };
            let ty = translate_ty(ty, ctxt);
            let v = with_string(name, |name| LLVMAddFunction(ctxt.llvm_mod, name, ty));
            ctxt.global_map.insert(gid, v);
        }

        // translate the functions.
        for (gid, d) in ctxt.m.global_defs.clone() {
            let GlobalDef::Function(name, ty, Some(imp)) = d else { continue; };
            translate_fn_impl(gid, imp, ctxt);
        }
    }
}

// TODO needs to be called in some functions context.
fn declare_str(i: GlobalValueId, ctxt: &mut Ctxt) {
    unsafe {
        let GlobalDef::String(s) = &ctxt.m.global_defs[&i] else { panic!() };
        let v = with_string(s, |s| LLVMBuildGlobalString(ctxt.builder, s, EMPTY));
        ctxt.global_map.insert(i, v);
    }
}

unsafe fn translate_fn_impl(gid: GlobalValueId, f: FnImpl, ctxt: &mut Ctxt) {
    unimplemented!() // TODO
}

unsafe fn translate_ty(ty: Type, ctxt: &mut Ctxt) -> LLVMTypeRef {
    unimplemented!() // TODO
}

fn with_string<T>(s: impl AsRef<str>, f: impl FnOnce(*const i8) -> T) -> T {
    let s = format!("{}\0", s.as_ref());
    f(s.as_bytes().as_ptr() as *const i8)
}

