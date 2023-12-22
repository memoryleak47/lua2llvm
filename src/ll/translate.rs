use std::collections::HashMap;

use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::target_machine::LLVMGetDefaultTargetTriple;

use crate::ll::*;
use Statement::*;
use Type::*;
use Expr::*;

const EMPTY: *const i8 = b"\0".as_ptr() as *const _;

pub fn dump(m: Module) {
    unsafe {
        let llvm_ctxt = LLVMContextCreate();
        let builder = LLVMCreateBuilderInContext(llvm_ctxt);
        let global_value_map = Default::default();

        let llvm_mod = with_string("luamod", |s| LLVMModuleCreateWithNameInContext(s, llvm_ctxt));
        let target = LLVMGetDefaultTargetTriple();
        LLVMSetTarget(llvm_mod, target);

        let mut ctxt = Ctxt {
            llvm_ctxt,
            builder,
            global_value_map,
            m,
            llvm_mod,

            block_map: Default::default(),
            var_map: Default::default(),
            local_value_map: Default::default(),
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
    global_value_map: HashMap<GlobalValueId, LLVMValueRef>,
    m: Module,
    llvm_mod: LLVMModuleRef,

    // function-local state
    block_map: HashMap<BlockId, LLVMBasicBlockRef>,
    var_map: HashMap<VarId, LLVMValueRef>,
    local_value_map: HashMap<LocalValueId, LLVMValueRef>,
}

unsafe fn translate(ctxt: &mut Ctxt) {
    unsafe {
        // declare all functions. String globals are declared on demand.
        for (gid, d) in ctxt.m.global_defs.clone() {
            let GlobalDef::Function(name, ty, _imp) = d else { continue; };
            let ty = translate_ty(ty, ctxt);
            let v = with_string(name, |name| LLVMAddFunction(ctxt.llvm_mod, name, ty));
            ctxt.global_value_map.insert(gid, v);
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
        ctxt.global_value_map.insert(i, v);
    }
}

unsafe fn translate_fn_impl(gid: GlobalValueId, f: FnImpl, ctxt: &mut Ctxt) {
    ctxt.block_map = Default::default();
    ctxt.var_map = Default::default();
    ctxt.local_value_map = Default::default();

    unsafe {
        let llvm_f: LLVMValueRef = ctxt.global_value_map[&gid];

        // alloc var block.
        let var_block = LLVMAppendBasicBlock(llvm_f, EMPTY);

        // init block map.
        for (&bid, blk) in &f.blocks {
            let b = LLVMAppendBasicBlock(llvm_f, EMPTY);
            ctxt.block_map.insert(bid, b);
        }

        // init var block.
        LLVMPositionBuilderAtEnd(ctxt.builder, var_block);
        for (&var_id, ty) in &f.vars {
            let ty = translate_ty(ty.clone(), ctxt);
            let var_v = LLVMBuildAlloca(ctxt.builder, ty, EMPTY);
            ctxt.var_map.insert(var_id, var_v);
        }
        LLVMBuildBr(ctxt.builder, ctxt.block_map[&f.start_block]);

        // translate blocks.
        for (bid, blk) in f.blocks {
            translate_block(blk, ctxt);
        }
    }
}

unsafe fn translate_block(block: Block, ctxt: &mut Ctxt) {
    unsafe {
        for st in block {
            match st {
                Compute(local_vid, expr) => {
                    let v = translate_expr(expr, ctxt);
                    ctxt.local_value_map.insert(local_vid, v);
                }
                PtrStore(val, ptr) => {
                    let val = translate_value(val, ctxt);
                    let ptr = translate_value(ptr, ctxt);
                    LLVMBuildStore(ctxt.builder, val, ptr);
                },
                Return(opt_val) => {
                    match opt_val {
                        Some(x) => {
                            let x = translate_value(x, ctxt);
                            LLVMBuildRet(ctxt.builder, x);
                        },
                        None => {
                            LLVMBuildRetVoid(ctxt.builder);
                        },
                    }
                }
                Unreachable => {
                    LLVMBuildUnreachable(ctxt.builder);
                },
                CondBr(cond, then_bid, else_bid) => {
                    let cond = translate_value(cond, ctxt);
                    let then_bid = ctxt.block_map[&then_bid];
                    let else_bid = ctxt.block_map[&else_bid];
                    LLVMBuildCondBr(ctxt.builder, cond, then_bid, else_bid);
                }
                Br(bid) => {
                    let bid = ctxt.block_map[&bid];
                    LLVMBuildBr(ctxt.builder, bid);
                }
                FnCall(f, args, ty) => {
                    let ty = translate_ty(ty, ctxt);
                    let f = translate_value(f, ctxt);
                    let mut args: Vec<LLVMValueRef> = args.into_iter().map(|x| translate_value(x, ctxt)).collect();
                    LLVMBuildCall2(ctxt.builder, ty, f, args.as_mut_ptr(), args.len() as u32, EMPTY);
                }
            }
        }
    }
}

unsafe fn translate_expr(expr: Expr, ctxt: &mut Ctxt) -> LLVMValueRef {
    unimplemented!()
}

unsafe fn translate_ty(ty: Type, ctxt: &mut Ctxt) -> LLVMTypeRef {
    unsafe {
        match ty {
            Pointer(ty) => {
                let ty = translate_ty(*ty, ctxt);
                LLVMPointerType(ty, 0)
            },
            Struct(sid) => {
                let elements: Vec<_> = ctxt.m.structs[&sid].iter().cloned().collect();
                let mut elements: Vec<_> = elements.into_iter().map(|x| translate_ty(x, ctxt)).collect();
                LLVMStructType(elements.as_mut_ptr(), elements.len() as u32, 0)
            },
            Function(ret, args) => {
                let ret = translate_ty(*ret, ctxt);
                let mut args: Vec<_> = args.into_iter().map(|x| translate_ty(x, ctxt)).collect();
                LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0)
            },
            Void => LLVMVoidType(),
            F64 => LLVMDoubleType(),
            I8 => LLVMInt8Type(),
            I32 => LLVMInt32Type(),
            I64 => LLVMInt64Type(),
            Bool => LLVMInt1Type(),
        }
    }
}

unsafe fn translate_value(vid: ValueId, ctxt: &mut Ctxt) -> LLVMValueRef {
    match vid {
        ValueId::Local(local) => ctxt.local_value_map[&local],
        ValueId::Global(global) => ctxt.global_value_map[&global],
    }
}

fn with_string<T>(s: impl AsRef<str>, f: impl FnOnce(*const i8) -> T) -> T {
    let s = format!("{}\0", s.as_ref());
    f(s.as_bytes().as_ptr() as *const i8)
}

