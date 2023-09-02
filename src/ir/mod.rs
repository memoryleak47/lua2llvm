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

#[derive(Debug, Clone)]
pub enum Statement {
    Compute(Node, Expr), // create a new node with the value returned from the Expr.
    Store(/*table: */ Node, /*index: */ Node, Node), // store the value from the Node in the table `table` at index `index`.
    If(Node, /*then*/ BlockId, /*else*/ BlockId),
    FnCall(/*func: */ Node, /* arg: */ Node),
    Command(Command),
    Return,
}

#[derive(Debug, Clone)]
pub enum Command {
    Print(Node),
    Throw(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Index(/*table: */ Node, /*index: */ Node),

    Arg,
    NewTable, // equivalent to {}
    LitFunction(FnId),
    BinOp(BinOpKind, Node, Node),
    Len(Node),
    Intrinsic(Intrinsic),

    // literals
    Num(f64),
    Bool(bool),
    Nil,
    Str(String),
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Next(Node, Node),
    Type(Node),
}

pub type Block = Vec<Statement>;
pub type BlockId = usize;

#[derive(Debug, Clone)]
pub struct LitFunction {
    pub blocks: Vec<Block>,
    pub start_block: BlockId,
}

#[derive(Debug, Default, Clone)]
pub struct IR {
    pub fns: Vec<LitFunction>,
    pub main_fn: FnId,
}


// UB definition:
//
// static: aka well-formedness
// - a Node is used, but there is a path through the CFG that doesn't initialize it
// - a single Node has multiple Compute statements
// - a FunctionId/BlockId is out of range
// - every Block should have exactly one Terminator (If / Throw / Return), it needs to be the final statement.
//
// runtime:
// - index / store into a non-table
// - function call to a non-function
// (- a If-node with a non-boolean argument) TODO maybe add this in the future, when truthyness is resolved in lowering.
// - division/remainder by zero?
// - store with index nil
// - argument to next is not a table
