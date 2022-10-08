use crate::ast::{BinOpKind, UnOpKind};

// Node is for temporary constants contained in the computation tree.
// Nodes are constructed using the Statement::Compute or Terminator::FnCall instructions.
// Each node id has exactly one such associated instruction. 
pub type Node = usize;

pub type GlobalId = usize;
pub type BlockId = usize;
pub type LocalId = usize;
pub type ClosureArgId = usize;
pub type FnArgId = usize;
pub type FnId = usize;

pub enum LocalVar {
    Local(LocalId), // literally variables created with the "local" keyword in this fn
    FnArg(FnArgId), // function arguments
    ClosureArg(ClosureArgId), // indexes into closure_args from parent function
}

// for mutable storage space
pub enum LValue {
    LocalVar(LocalVar),
    Global(GlobalId),
    Index(/*table: */ Node, /*idx: */ Node),
    // both table and idx will be evaluated to a Value, left-side needs to be Value::TablePtr.
}

pub enum Statement {
    Compute(Node, Expr), // create a new node with the value returned from the Expr.
    Store(LValue, Node), // store the value from the Node in the LValue
}

pub enum Expr {
    LValue(LValue),
    Num(f64),
    Bool(bool),
    Nil,
    Str(String),
    NewTable, // equivalent to {}
    LitFunction(/*fn-id: */ FnId), // information about this in LitFunctionData
    BinOp(BinOpKind, Node, Node),
    UnOp(UnOpKind, Node, Node),
}

pub struct LitFunctionData {
    pub no_args: usize,
    pub closure_args: Vec<LocalVar>, // closure_args[i] needs to be evaluated in the parent function, and is usable inside the inner function as LocalVar::ClosureArg(i)
    pub blocks: Vec<Block>,
    pub start_block: BlockId,
    pub parent_fn: Option<FnId>,
}

pub struct Block {
    pub body: Vec<Statement>,
    pub term: Terminator,
}

pub enum Terminator {
    If(Node, /*if-block*/ BlockId, /*else-block*/ BlockId), // goes to if-block if node contains truthy value, otherwise else block
    GoTo(/*block-id: */ BlockId),
    FnCall(/*fn: */ Node, /*args: */ Node, /*return-place: */ Node, /*block-afterwards: */ BlockId),
    Return(Node),
}

pub struct IR {
    pub fns: Vec<LitFunctionData>,
    pub main_fn: FnId,
}
