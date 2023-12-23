use crate::ll::*;

pub struct Builder {
    pub m: Module,
    active_fn: Option<ActiveFn>,
}

struct ActiveFn {
    gid: GlobalValueId,
    next_local_value_id: LocalValueId,
    active_block: Option<BlockId>,
    start_block_defined: bool,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            m: Module::default(),
            active_fn: None,
        }
    }

    pub fn finish(mut self) -> Module {
        self.finish_active_fn();

        self.m
    }

    pub fn alloc_struct(&mut self, elements: Vec<Type>) -> StructId {
        let n = StructId(self.m.structs.len());
        self.m.structs.insert(n, elements);

        n
    }

    pub fn alloc_fn(&mut self, name: impl Into<String>, ty: Type) -> ValueId {
        self.alloc_global_def(GlobalDef::Function(name.into(), ty, None))
    }

    pub fn alloc_string(&mut self, s: impl Into<String>) -> ValueId {
        self.alloc_global_def(GlobalDef::String(s.into()))
    }

    pub fn alloc_block(&mut self) -> BlockId {
        let (_, i) = self.current_impl();
        let n = BlockId(i.blocks.len());
        i.blocks.insert(n, vec![]);

        n
    }

    pub fn alloc_var(&mut self, ty: Type) -> VarId {
        let (_, i) = self.current_impl();
        let n = VarId(i.vars.len());
        i.vars.insert(n, ty);
        n
    }

    pub fn set_active_fn(&mut self, gid: ValueId) {
        self.finish_active_fn();

        let ValueId::Global(gid) = gid else { panic!("argument to set_active_fn is no GlobalValueId") };
        let Some(GlobalDef::Function(_, _, i)) = &mut self.m.global_defs.get_mut(&gid) else { panic!("argument to set_active_fn is no function!") };

        if i.is_some() {
            panic!("Attempt to implement a function twice!");
        }

        *i = Some(FnImpl {
            vars: Default::default(),
            blocks: Default::default(),
            start_block: BlockId(0),
        });

        self.active_fn = Some(ActiveFn {
            gid,
            next_local_value_id: LocalValueId(0),
            active_block: None,
            start_block_defined: false,
        });
    }

    pub fn set_active_block(&mut self, bid: BlockId) {
        let (a, _) = self.current_impl();
        a.active_block = Some(bid);
    }

    pub fn set_start_block(&mut self, bid: BlockId) {
        let (a, i) = self.current_impl();

        if a.start_block_defined { panic!("start block was already defined!"); }

        i.start_block = bid;
        a.start_block_defined = true;
       
    }

    fn finish_active_fn(&mut self) {
        if let Some(f) = &self.active_fn {
            if !f.start_block_defined {
                panic!("Start block has not been defined!");
            }
        }

        self.active_fn = None;
    }

    fn alloc_global_def(&mut self, def: GlobalDef) -> ValueId {
        let n = GlobalValueId(self.m.global_defs.len());
        self.m.global_defs.insert(n, def);

        ValueId::Global(n)
    }

    fn current_impl(&mut self) -> (&mut ActiveFn, &mut FnImpl) {
        let Some(active) = &mut self.active_fn else {
            panic!("current_impl called while no function was active!");
        };
        let Some(GlobalDef::Function(_, _, Some(i))) = self.m.global_defs.get_mut(&active.gid) else {
            panic!("current_impl called in invalid state!");
        };

        (active, i)
    }

    pub fn push_st(&mut self, st: Statement) {
        let (a, i) = self.current_impl();
        let b = a.active_block.expect("push_st requires an active block!");
        i.blocks.get_mut(&b).unwrap().push(st);
    }

    pub fn push_compute(&mut self, expr: Expr) -> ValueId {
        let (a, _) = self.current_impl();

        let n = a.next_local_value_id;
        a.next_local_value_id.0 += 1;

        self.push_st(Statement::Compute(n, expr));

        ValueId::Local(n)
    }
}

///////////////////////////////////////////////////
// convenience methods for all Statements / Exprs.
///////////////////////////////////////////////////

#[allow(dead_code)]
impl Builder {

    // statements
    pub fn push_ptr_store(&mut self, val: ValueId, ptr: ValueId) {
        self.push_st(Statement::PtrStore(val, ptr));
    }

    pub fn push_return(&mut self, val: ValueId) {
        self.push_st(Statement::Return(Some(val)));
    }

    pub fn push_return_void(&mut self) {
        self.push_st(Statement::Return(None));
    }

    pub fn push_unreachable(&mut self) {
        self.push_st(Statement::Unreachable);
    }

    pub fn push_cond_br(&mut self, cond: ValueId, then: BlockId, else_: BlockId) {
        self.push_st(Statement::CondBr(cond, then, else_));
    }

    pub fn push_br(&mut self, target: BlockId) {
        self.push_st(Statement::Br(target));
    }

    // exprs

    pub fn push_fn_call(&mut self, call: ValueId, args: impl AsRef<[ValueId]>, ty: Type) -> ValueId {
        let args = args.as_ref().iter().cloned().collect();
        self.push_compute(Expr::FnCall(call, args, ty))
    }

    pub fn push_i_plus(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Plus, NumKind::Int, l, r)) }
    pub fn push_i_minus(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Minus, NumKind::Int, l, r)) }
    pub fn push_i_mul(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Mul, NumKind::Int, l, r)) }
    pub fn push_i_div(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Div, NumKind::Int, l, r)) }
    pub fn push_i_mod(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Mod, NumKind::Int, l, r)) }
    pub fn push_i_lt(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Lt, NumKind::Int, l, r)) }
    pub fn push_i_le(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Le, NumKind::Int, l, r)) }
    pub fn push_i_gt(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Gt, NumKind::Int, l, r)) }
    pub fn push_i_ge(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Ge, NumKind::Int, l, r)) }
    pub fn push_i_is_equal(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::IsEqual, NumKind::Int, l, r)) }
    pub fn push_i_is_not_equal(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::IsNotEqual, NumKind::Int, l, r)) }

    pub fn push_f_plus(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Plus, NumKind::Float, l, r)) }
    pub fn push_f_minus(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Minus, NumKind::Float, l, r)) }
    pub fn push_f_mul(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Mul, NumKind::Float, l, r)) }
    pub fn push_f_div(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Div, NumKind::Float, l, r)) }
    pub fn push_f_mod(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Mod, NumKind::Float, l, r)) }
    pub fn push_f_lt(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Lt, NumKind::Float, l, r)) }
    pub fn push_f_le(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Le, NumKind::Float, l, r)) }
    pub fn push_f_gt(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Gt, NumKind::Float, l, r)) }
    pub fn push_f_ge(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::Ge, NumKind::Float, l, r)) }
    pub fn push_f_is_equal(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::IsEqual, NumKind::Float, l, r)) }
    pub fn push_f_is_not_equal(&mut self, l: ValueId, r: ValueId) -> ValueId { self.push_compute(Expr::NumOp(NumOpKind::IsNotEqual, NumKind::Float, l, r)) }

    pub fn push_ptr_load(&mut self, ty: Type, ptr: ValueId) -> ValueId {
        self.push_compute(Expr::PtrLoad(ty, ptr))
    }

    pub fn push_not(&mut self, x: ValueId) -> ValueId {
        self.push_compute(Expr::Not(x))
    }

    pub fn push_or(&mut self, l: ValueId, r: ValueId) -> ValueId {
        self.push_compute(Expr::Or(l, r))
    }

    pub fn push_var(&mut self, var_id: VarId) -> ValueId {
        self.push_compute(Expr::Var(var_id))
    }

    pub fn push_arg(&mut self, i: usize) -> ValueId {
        self.push_compute(Expr::Arg(i))
    }

    pub fn push_ptr_to_int(&mut self, v: ValueId, ty: Type) -> ValueId { self.push_compute(Expr::PtrToInt(v, ty)) }
    pub fn push_int_to_ptr(&mut self, v: ValueId, ty: Type) -> ValueId { self.push_compute(Expr::IntToPtr(v, ty)) }
    pub fn push_bit_cast(&mut self, v: ValueId, ty: Type) -> ValueId { self.push_compute(Expr::BitCast(v, ty)) }
    pub fn push_zext(&mut self, v: ValueId, ty: Type) -> ValueId { self.push_compute(Expr::ZExt(v, ty)) }

    pub fn push_extract_value(&mut self, s: ValueId, i: usize) -> ValueId {
        self.push_compute(Expr::ExtractValue(s, i))
    }

    pub fn push_insert_value(&mut self, s: ValueId, v: ValueId, i: usize) -> ValueId {
        self.push_compute(Expr::InsertValue(s, v, i))
    }

    pub fn push_poison(&mut self, ty: Type) -> ValueId {
        self.push_compute(Expr::Poison(ty))
    }

    pub fn push_const_real(&mut self, ty: Type, v: f64) -> ValueId {
        self.push_compute(Expr::ConstReal(ty, v))
    }

    pub fn push_const_int(&mut self, ty: Type, v: i64) -> ValueId {
        self.push_compute(Expr::ConstInt(ty, v))
    }
}
