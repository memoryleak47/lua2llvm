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

    // TODO add convenience methods for all Statements / Exprs.
}
