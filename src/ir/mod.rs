pub use crate::ast::UnOpKind;

mod display;

// the same as ast::BinOpKind but without And & Or.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpKind {
    Plus, Minus, Mul, Div, Mod,
    Lt, Le, Gt, Ge,
    IsEqual, IsNotEqual,
    Concat, Pow,
}

// Node is for temporary constants contained in the computation tree.
// Nodes are constructed using the Statement::Compute instruction.
// Each node id has exactly one such associated instruction. 
// nodes are somewhat like virtual registers %<id> from LLVM IR.
pub type Node = usize;

// a local variable: usable from within the body of it's function.
pub type LocalId = usize;

// a global variable.
pub type GlobalId = usize;

// used to index into IR::fns.
pub type FnId = usize;

pub type UpvalueId = usize;

// for mutable storage space
#[derive(Debug, Clone)]
pub enum LValue {
    // literally variables created with the "local" keyword in this fn
    Local(LocalId),

    // local variables from some outer function
    Upvalue(UpvalueId),

    Global(GlobalId),

    // both table and idx will be evaluated to a Value, left-side needs to be Value::TablePtr.
    Index(/*table: */ Node, /*idx: */ Node),
}

#[derive(Debug)]
pub enum Statement {
    // if you closure a local variable, it's relevant where the "local" lies, (i.e. in or out of a given loop)
    // because each further "local" call creates a fresh object.
    Local(LocalId),
    Compute(Node, Expr), // create a new node with the value returned from the Expr.
    Store(LValue, Node), // store the value from the Node in the LValue
    ReturnTable(Node), // Node needs to be a table
    If(Node, /*then*/ Vec<Statement>, /*else*/ Vec<Statement>),
    Loop(Vec<Statement>), // loops until a break happens
    Break,
}

#[derive(Debug)]
pub enum Expr {
    LValue(LValue),
    Argtable, // the table where all function arguments are stored in sequentially
    FnCall(/*func: */ Node, /* input-table: */ Node),
    NewTable, // equivalent to {}
    LitFunction(FnId),
    BinOp(BinOpKind, Node, Node),
    UnOp(UnOpKind, Node),

    // literals
    Num(f64),
    Bool(bool),
    Nil,
    Str(String),
}

#[derive(Debug)]
pub struct LitFunction {
    pub body: Vec<Statement>,

    // upvalue_refs[i] is accessible as LValue::Upvalue(i) within this function.
    // the UpvalueRef upvalue_refs[i] needs to be evaluated to a Value when this function is instantiated.
    // The resulting Value will always be a TablePtr.
    pub upvalue_refs: Vec<UpvalueRef>,
}

// you can only closure local & already closured variables.
#[derive(Debug, Clone)]
pub enum UpvalueRef {
    Local(LocalId),
    Upvalue(UpvalueId),
}

#[derive(Debug, Default)]
pub struct IR {
    pub fns: Vec<LitFunction>,
    pub main_fn: FnId,
    pub globals: Vec<String>,
}
