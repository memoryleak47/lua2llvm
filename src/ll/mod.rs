mod translate;

use std::collections::HashMap;

// My own LLVM front end.
// It exposes a subset of the llvm-sys C API, but with a rusty API.

// XXX My first and foremost goal is to use this in lua2llvm.
// Hence I'll ignore all features that I won't use right away.

pub type StructId = usize;
pub type BlockId = usize;
pub type VarId = usize;

pub type GlobalValueId = usize;
pub type LocalValueId = usize;

#[derive(Clone, Copy)]
pub enum ValueId {
    Global(GlobalValueId), // global. Typically represented by @i
    Local(LocalValueId), // local to a function. Typically represented by %i
}

#[derive(Clone)]
pub struct Module {
    pub structs: HashMap<StructId, Vec<Type>>,
    pub global_defs: HashMap<GlobalValueId, GlobalDef>
}

#[derive(Clone)]
pub enum GlobalDef {
    // functions have no special ids in this model. Just GlobalValueIds.
    Function(/*fname:*/ String, /*ftype:*/ Type, Option<FnImpl>),
    String(String),
}

pub type Block = Vec<Statement>;

#[derive(Clone)]
pub struct FnImpl {
    pub vars: HashMap<VarId, Type>,
    pub blocks: HashMap<BlockId, Block>,
    pub start_block: BlockId,
}

#[derive(Clone)]
pub enum Statement {
    Compute(ValueId, Expr),
    PtrStore(/*val: */ ValueId, /*ptr: */ ValueId),
    Return(Option<ValueId>),
    Unreachable,
    CondBr(ValueId, /*then: */ BlockId, /*else: */ BlockId),
    Br(BlockId),
    FnCall(/*fn: */ ValueId, /*args: */ Vec<ValueId>, /*ftype: */ Type),
}

#[derive(Clone)]
pub enum NumOpKind {
    Plus, Minus, Mul, Div, Mod,
    Lt, Le, Gt, Ge,
    IsEqual, IsNotEqual,
}

#[derive(Clone)]
pub enum NumKind { Int, Float }

#[derive(Clone)]
pub enum Expr {
    NumOp(NumOpKind, NumKind, ValueId, ValueId),

    PtrLoad(/*ty: */ Type, /*ptr: */ ValueId),

    // boolean ops
    Not(ValueId),
    Or(ValueId, ValueId),

    Var(VarId), // returns a pointer to its data.
    Arg(usize),

    // casting
    PtrToInt(ValueId, Type),
    IntToPtr(ValueId, Type),
    BitCast(ValueId, Type),

    ExtractValue(/*struct*/ ValueId, usize),
    InsertValue(/*struct*/ ValueId, /*val: */ ValueId, usize),
    Poison(/*ty*/ Type), // we prefer Poison over Undef.

    ConstReal(/*ty: */ Type, /*value: */ f64),
    ConstInt(/*ty: */ Type, /*value: */ i64),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Pointer(Box<Type>),
    Struct(StructId),
    Function(/*ret*/ Box<Type>, /*args*/ Vec<Type>),
    Void,
    F64,
    I8,
    I32,
    I64,
    Bool,
}
