pub use crate::ast::UnOpKind;

mod display;

// Note that even though, lower.rs only returns tables from functions, and Arg is always a table too.
// This is no constraint for the IR itself.
// Further, upvalues might not be tables, but can be any Value (which will be closured per value).

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
// nodes are local to the functions they are defined in.
pub type Node = usize;

// used to index into IR::fns.
pub type FnId = usize;

pub type UpvalueId = usize;

#[derive(Debug)]
pub enum Statement {
    Compute(Node, Expr), // create a new node with the value returned from the Expr.
    Store(/*table: */ Node, /*index: */ Node, Node), // store the value from the Node in the table `table` at index `index`.
    Return(Node),
    If(Node, /*then*/ Vec<Statement>, /*else*/ Vec<Statement>),
    Loop(Vec<Statement>), // loops until a break happens
    Break,
}

#[derive(Debug)]
pub enum Expr {
    Index(/*table: */ Node, /*index: */ Node),
    // Upvalue(i) is no LValue as it's only used as Upvalue(i)[1].
    Upvalue(UpvalueId),

    Arg,
    FnCall(/*func: */ Node, /* arg: */ Node),
    NewTable, // equivalent to {}
    LitFunction(FnId, /*upnodes: */ Vec<Node>),
    NativeFn(/*name: */ String),
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
    pub body: Vec<Statement>
}

#[derive(Debug, Default)]
pub struct IR {
    pub fns: Vec<LitFunction>,
    pub main_fn: FnId,
}
