use super::*;

pub struct Ctxt {
    pub b: ll::Builder,

    // these are cloned in for now!
    pub ir: IR,
    pub inf: Infer,
    pub layout: Layout, // TODO respect the layouting information

    pub lit_fns: HashMap<FnId, ll::ValueId>,
    pub extra_fns: HashMap<String, ll::ValueId>,

    pub value_struct_id: Option<ll::StructId>,

    // function-local:
    pub current_fid: FnId,
    pub current_bid: BlockId,
    pub blocks: HashMap<BlockId, ll::BlockId>,
    pub nodes: HashMap<Node, ll::ValueId>,
}

impl Ctxt {
    pub fn new(ir: &IR, inf: &Infer, layout: &Layout) -> Self {
        Ctxt {
            b: ll::Builder::new(),

            ir: ir.clone(),
            inf: inf.clone(),
            layout: layout.clone(),

            lit_fns: Default::default(),
            extra_fns: Default::default(),

            value_struct_id: None,

            current_fid: 0,
            current_bid: 0,
            blocks: Default::default(),
            nodes: Default::default(),
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
        ll::Type::Struct(self.value_struct_id.unwrap())
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
}
