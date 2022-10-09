use crate::ast::UnOpKind;

// the same as ast::BinOpKind but without And & Or.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpKind {
    Plus, Minus, Mul, Div, Mod,
    Lt, Le, Gt, Ge,
    IsEqual, IsNotEqual,
    Concat, Pow,
}

// Node is for temporary constants contained in the computation tree.
// Nodes are constructed using the Statement::Compute or Statement::FnCall instructions.
// Each node id has exactly one such associated instruction. 
// nodes are somewhat like virtual registers %<id> from LLVM IR.
pub type Node = usize;

// a local variable: usable from within the body of it's function.
pub type LocalId = usize;

// a variable being a local variable in some super function,
// in whose body this child function was defined.
pub type ClosureArgId = usize;

#[derive(Debug)]
pub enum LocalVar {
    Local(LocalId), // literally variables created with the "local" keyword in this fn
    ClosureArg(ClosureArgId), // indexes into closure_args from parent function
}

// for mutable storage space
#[derive(Debug)]
pub enum LValue {
    LocalVar(LocalVar),
    Index(/*table: */ Node, /*idx: */ Node),
    // both table and idx will be evaluated to a Value, left-side needs to be Value::TablePtr.
}

#[derive(Debug)]
pub enum Statement {
    Compute(Node, Expr), // create a new node with the value returned from the Expr.
    Store(LValue, Node), // store the value from the Node in the LValue
    FnCall(/*func: */ Node, /* input-table: */ Node),
    ReturnTable(Node), // Node needs to be a table
    If(Node, /*then*/ Vec<Statement>, /*else*/ Vec<Statement>),
    Loop(Vec<Statement>), // loops until a break happens
    Break,
}

#[derive(Debug)]
pub enum Expr {
    LValue(LValue),
    Argtable, // the table where all function arguments are stored in sequentially
    Num(f64),
    Bool(bool),
    Nil,
    Str(String),
    FnCall(/*func: */ Node, /* input-table: */ Node),
    NewTable, // equivalent to {}
    LitFunction(LitFunction),
    BinOp(BinOpKind, Node, Node),
    UnOp(UnOpKind, Node, Node),
}

#[derive(Debug)]
pub struct LitFunction {
    // closure_args[i] needs to be evaluated in the parent function,
    // and is usable inside the inner function as LocalVar::ClosureArg(i)
    pub closure_args: Vec<LocalVar>,
    pub body: Vec<Statement>,

    // number of local variables
    pub local_count: usize,
}

#[derive(Debug)]
pub struct IR {
    pub main_fn: LitFunction,
}
