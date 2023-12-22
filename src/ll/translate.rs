use std::collections::{HashMap, HashSet};

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
            strings_done: false,

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

    strings_done: bool, // expresses whether global strings have already been declared.

    // function-local state
    block_map: HashMap<BlockId, LLVMBasicBlockRef>,
    var_map: HashMap<VarId, LLVMValueRef>,
    local_value_map: HashMap<LocalValueId, LLVMValueRef>,
}

unsafe fn translate(ctxt: &mut Ctxt) {
    unsafe {
        let mut gdefs: Vec<_> = ctxt.m.global_defs.iter().map(|x| (x.0.clone(), x.1.clone())).collect();
        gdefs.sort_by_key(|x| x.0.0);

        // declare all functions. String globals are declared later.
        for (gid, d) in gdefs.clone() {
            let GlobalDef::Function(name, ty, _imp) = d else { continue; };
            let ty = translate_ty(ty, ctxt);
            let v = with_string(name, |name| LLVMAddFunction(ctxt.llvm_mod, name, ty));
            ctxt.global_value_map.insert(gid, v);
        }

        // translate the functions.
        for (gid, d) in gdefs {
            let GlobalDef::Function(_, _, Some(imp)) = d else { continue; };
            translate_fn_impl(gid, imp, ctxt);
        }
    }
}

unsafe fn declare_strings(ctxt: &mut Ctxt) {
    unsafe {
        for (gid, def) in &ctxt.m.global_defs {
            let GlobalDef::String(s) = def else { continue };
            let v = with_string(s, |s| LLVMBuildGlobalString(ctxt.builder, s, EMPTY));
            ctxt.global_value_map.insert(*gid, v);
        }
    }
}

// LLVM requires blocks to be calculated in the correct order.
// The ll module doesn't have this constraint.
fn llvm_block_order(f: &FnImpl) -> Vec<BlockId> {
    let mut open = HashSet::new();
    let mut done = HashSet::new();
    let mut done_vec = vec![];

    open.insert(f.start_block);

    // the .min() keeps the original order as best as possible.
    while let Some(&x) = open.iter().min() {
        open.remove(&x);
        done.insert(x);
        done_vec.push(x);

        for e in out_edges(f, x) {
            if !done.contains(&e) {
                open.insert(e);
            }
        }
    }

    done_vec
}

fn out_edges(f: &FnImpl, bid: BlockId) -> Vec<BlockId> {
    for st in &f.blocks[&bid] {
        match st {
            Statement::CondBr(_, x, y) => return vec![*x, *y],
            Statement::Br(x) => return vec![*x],
            _ => {},
        }
    }

    vec![]
}

unsafe fn translate_fn_impl(gid: GlobalValueId, f: FnImpl, ctxt: &mut Ctxt) {
    ctxt.block_map = Default::default();
    ctxt.var_map = Default::default();
    ctxt.local_value_map = Default::default();

    unsafe {
        let llvm_f: LLVMValueRef = ctxt.global_value_map[&gid];

        // alloc var block.
        let var_block = LLVMAppendBasicBlock(llvm_f, EMPTY);

        let bids: Vec<BlockId> = llvm_block_order(&f);

        // init block map.
        for &bid in &bids {
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
        for &bid in &bids {
            let blk = &f.blocks[&bid];
            translate_block(bid, blk.clone(), llvm_f, ctxt);
        }
    }
}

unsafe fn translate_block(bid: BlockId, block: Block, llvm_f: LLVMValueRef, ctxt: &mut Ctxt) {
    unsafe {
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.block_map[&bid]);

        if !ctxt.strings_done {
            declare_strings(ctxt);
            ctxt.strings_done = true;
        }

        for st in block {
            match st {
                Compute(local_vid, expr) => {
                    let v = translate_expr(expr, llvm_f, ctxt);
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
            }
        }
    }
}

unsafe fn translate_expr(expr: Expr, llvm_f: LLVMValueRef, ctxt: &mut Ctxt) -> LLVMValueRef {
    unsafe {
        match expr {
            FnCall(f, args, ty) => {
                let ty = translate_ty(ty, ctxt);
                let f = translate_value(f, ctxt);
                let mut args: Vec<LLVMValueRef> = args.into_iter().map(|x| translate_value(x, ctxt)).collect();
                LLVMBuildCall2(ctxt.builder, ty, f, args.as_mut_ptr(), args.len() as u32, EMPTY)
            }
            NumOp(op_kind, num_kind, v1, v2) => {
                use NumKind::*;
                use NumOpKind::*;

                use llvm_sys::LLVMRealPredicate::*;
                use llvm_sys::LLVMIntPredicate::*;

                let v1 = translate_value(v1, ctxt);
                let v2 = translate_value(v2, ctxt);
                match (num_kind, op_kind) {
                    (Float, Plus) => LLVMBuildFAdd(ctxt.builder, v1, v2, EMPTY),
                    (Float, Minus) => LLVMBuildFSub(ctxt.builder, v1, v2, EMPTY),
                    (Float, Mul) => LLVMBuildFMul(ctxt.builder, v1, v2, EMPTY),
                    (Float, Div) => LLVMBuildFDiv(ctxt.builder, v1, v2, EMPTY),
                    (Float, Mod) => LLVMBuildFRem(ctxt.builder, v1, v2, EMPTY),
                    (Float, IsEqual) => LLVMBuildFCmp(ctxt.builder, LLVMRealOEQ, v1, v2, EMPTY),
                    (Float, IsNotEqual) => LLVMBuildFCmp(ctxt.builder, LLVMRealONE, v1, v2, EMPTY),
                    (Float, Lt) => LLVMBuildFCmp(ctxt.builder, LLVMRealOLT, v1, v2, EMPTY),
                    (Float, Gt) => LLVMBuildFCmp(ctxt.builder, LLVMRealOGT, v1, v2, EMPTY),
                    (Float, Le) => LLVMBuildFCmp(ctxt.builder, LLVMRealOLE, v1, v2, EMPTY),
                    (Float, Ge) => LLVMBuildFCmp(ctxt.builder, LLVMRealOGE, v1, v2, EMPTY),

                    (Int, Plus) => LLVMBuildAdd(ctxt.builder, v1, v2, EMPTY),
                    (Int, Minus) => LLVMBuildSub(ctxt.builder, v1, v2, EMPTY),
                    (Int, Mul) => LLVMBuildMul(ctxt.builder, v1, v2, EMPTY),
                    (Int, Div) => LLVMBuildSDiv(ctxt.builder, v1, v2, EMPTY),
                    (Int, Mod) => LLVMBuildSRem(ctxt.builder, v1, v2, EMPTY),
                    (Int, IsEqual) => LLVMBuildICmp(ctxt.builder, LLVMIntEQ, v1, v2, EMPTY),
                    (Int, IsNotEqual) => LLVMBuildICmp(ctxt.builder, LLVMIntNE, v1, v2, EMPTY),
                    (Int, Lt) => LLVMBuildICmp(ctxt.builder, LLVMIntSLT, v1, v2, EMPTY),
                    (Int, Gt) => LLVMBuildICmp(ctxt.builder, LLVMIntSGT, v1, v2, EMPTY),
                    (Int, Le) => LLVMBuildICmp(ctxt.builder, LLVMIntSLE, v1, v2, EMPTY),
                    (Int, Ge) => LLVMBuildICmp(ctxt.builder, LLVMIntSGE, v1, v2, EMPTY),
                }
            },

            PtrLoad(ty, ptr) => {
                let ty = translate_ty(ty, ctxt);
                let ptr = translate_value(ptr, ctxt);
                LLVMBuildLoad2(ctxt.builder, ty, ptr, EMPTY)
            },

            Not(v) => {
                let v = translate_value(v, ctxt);
                LLVMBuildNot(ctxt.builder, v, EMPTY)
            },
            Or(v1, v2) => {
                let v1 = translate_value(v1, ctxt);
                let v2 = translate_value(v2, ctxt);
                LLVMBuildOr(ctxt.builder, v1, v2, EMPTY)
            }

            Var(var_id) => {
                ctxt.var_map[&var_id]
            },
            Arg(i) => {
                LLVMGetParam(llvm_f, i as u32)
            }

            // casting
            PtrToInt(v, ty) => {
                let v = translate_value(v, ctxt);
                let ty = translate_ty(ty, ctxt);
                LLVMBuildPtrToInt(ctxt.builder, v, ty, EMPTY)
            },
            IntToPtr(v, ty) => {
                let v = translate_value(v, ctxt);
                let ty = translate_ty(ty, ctxt);
                LLVMBuildIntToPtr(ctxt.builder, v, ty, EMPTY)
            },
            BitCast(v, ty) => {
                let v = translate_value(v, ctxt);
                let ty = translate_ty(ty, ctxt);
                LLVMBuildBitCast(ctxt.builder, v, ty, EMPTY)
            },
            ZExt(v, ty) => {
                let v = translate_value(v, ctxt);
                let ty = translate_ty(ty, ctxt);
                LLVMBuildZExt(ctxt.builder, v, ty, EMPTY)
            },

            ExtractValue(s, i) => {
                let s = translate_value(s, ctxt);
                LLVMBuildExtractValue(ctxt.builder, s, i as u32, EMPTY)
            },
            InsertValue(s, v, i) => {
                let s = translate_value(s, ctxt);
                let v = translate_value(v, ctxt);
                LLVMBuildInsertValue(ctxt.builder, s, v, i as u32, EMPTY)
            },
            Poison(ty) => {
                let ty = translate_ty(ty, ctxt);
                LLVMGetPoison(ty)
            },

            ConstReal(ty, x) => {
                let ty = translate_ty(ty, ctxt);
                LLVMConstReal(ty, x)
            },
            ConstInt(ty, x) => {
                let ty = translate_ty(ty, ctxt);
                LLVMConstInt(ty, x.try_into().unwrap(), 0)
            },
        }
    }
    
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

