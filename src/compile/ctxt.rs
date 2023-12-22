use super::*;

pub struct Ctxt {
    pub m: ll::Module,

    // these are cloned in for now!
    pub ir: IR,
    pub inf: Infer,
    pub layout: Layout, // TODO respect the layouting information

    pub lit_fns: HashMap<FnId, ll::GlobalValueId>,
    pub extra_fns: HashMap<String, ll::GlobalValueId>,

    pub start_fn: ll::GlobalValueId,

    // function-local:
    pub current_fid: FnId,
    pub current_bid: BlockId,
    pub blocks: HashMap<BlockId, ll::BlockId>,
    pub nodes: HashMap<Node, ll::ValueId>,

    // information relevant to practically mutate the module m.
    // TODO extract this into a ll::ModuleWriter.
    pub current_ll_fn: ll::GlobalValueId,
    pub current_ll_bid: ll::BlockId,
    pub next_local_value_id: ll::LocalValueId,
}

impl Ctxt {
    pub fn new(ir: &IR, inf: &Infer, layout: &Layout) -> Self {
        Ctxt {
            m: ll::Module::default(),

            ir: ir.clone(),
            inf: inf.clone(),
            layout: layout.clone(),

            lit_fns: Default::default(),
            extra_fns: Default::default(),

            // these need to be set later on!
            start_fn: ll::GlobalValueId(0),

            current_fid: 0,
            current_bid: 0,
            blocks: Default::default(),
            nodes: Default::default(),

            current_ll_fn: ll::GlobalValueId(0),
            current_ll_bid: ll::BlockId(0),
            next_local_value_id: ll::LocalValueId(0),
        }
    }

    pub fn i32_t(&self) -> ll::Type {
        ll::Type::I32
    }

    pub fn i64_t(&self) -> ll::Type {
        ll::Type::I64
    }

    pub fn void_t(&self) -> ll::Type {
        ll::Type::Void
    }

    pub fn f64_t(&self) -> ll::Type {
        ll::Type::F64
    }

    pub fn value_t(&self) -> ll::Type {
        ll::Type::Struct(ll::StructId(0))
    }

    pub fn value_ptr_t(&self) -> ll::Type {
        ll::Type::Pointer(Box::new(self.value_t()))
    }

    pub fn v2void_t(&self) -> ll::Type {
        ll::Type::Function(Box::new(self.void_t()), vec![self.value_ptr_t()])
    }

    pub fn v2void_ptr_t(&self) -> ll::Type {
        ll::Type::Pointer(Box::new(self.v2void_t()))
    }

    pub fn i8_t(&self) -> ll::Type {
        ll::Type::I8
    }

    pub fn bool_t(&self) -> ll::Type {
        ll::Type::Bool
    }

    pub fn str_t(&self) -> ll::Type {
        ll::Type::Pointer(Box::new(self.i8_t()))
    }

    pub fn alloc_block(&mut self) -> ll::BlockId {
        let f = self.current_fn_impl();
        let bid = ll::BlockId(f.blocks.len());
        f.blocks.insert(bid, vec![]);
        bid
    }

    pub fn alloc_fn(&mut self, name: String, ty: ll::Type) -> ll::GlobalValueId {
        let def = ll::GlobalDef::Function(name, ty, None);
        self.alloc_global(def)
    }

    pub fn alloc_global(&mut self, def: ll::GlobalDef) -> ll::GlobalValueId {
        let n = ll::GlobalValueId(self.m.global_defs.len());
        self.m.global_defs.insert(n, def);
        n
    }

    pub fn push_st(&mut self, st: ll::Statement) {
        let current_ll_bid = self.current_ll_bid;
        let stmts = self.current_fn_impl().blocks.get_mut(&current_ll_bid).unwrap();
        stmts.push(st);
    }

    pub fn push_compute(&mut self, expr: ll::Expr) -> ll::ValueId {
        let n = self.next_local_value_id;
        self.next_local_value_id.0 += 1;

        self.push_st(ll::Statement::Compute(n, expr));

        ll::ValueId::Local(n)
    }

    pub fn current_fn_impl(&mut self) -> &mut ll::FnImpl {
        let gid = self.current_ll_fn;
        let Some(ll::GlobalDef::Function(_, _, x)) = self.m.global_defs.get_mut(&gid) else { panic!("no active function?") };
        let x: &mut Option<_> = x;
        if x.is_none() {
            *x = Some(ll::FnImpl {
                vars: Default::default(),
                blocks: Default::default(),
                start_block: ll::BlockId(0),
            });
        }

        match x {
            Some(x) => x,
            None => unreachable!(),
        }
    }
}
