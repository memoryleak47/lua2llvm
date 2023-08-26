mod table;
use table::*;

mod native_fn;
use native_fn::*;

mod expr;
use expr::*;

mod fun;
use fun::*;

mod body;
use body::*;

pub(self) use std::collections::HashMap;

pub(self) use crate::ast::*;
pub(self) use crate::ir::{self, FnId, IR, LitFunction, Node};

#[derive(Default)]
pub(self) struct Ctxt {
    ir: IR,

    // the Vec<> is pushed() & popped() for blocks, NOT functions.
    // The node `locals[ident]` contains a TablePtr with the single index 1.
    // upvalues are stored within here aswell, they store a table pointer to the variable in the stack frame where the upvalues originally came from.
    locals: Vec<HashMap<String, Node>>,

    // All idents that were used in this function, even though there were not locally defined.
    upvalue_idents: Vec<String>,

    // this is intended to be std::mem::swap'ped out, when needed.
    body: Vec<ir::Statement>,
    next_node: usize,

    is_main: bool,

    // Some(_) for variadics, None otherwise.
    ellipsis_node: Option<Node>,

    zero: Node,
    one: Node,
}

// checks whether `arg` is a function, returns this in a new node as bool value.
fn mk_fn_check(arg: Node, ctxt: &mut Ctxt) -> (/*bool node*/ Node, /*call Node*/ Node) {
    // return intrinsic::type(arg) == "table" && intrinsic::type(arg["call"]) != "function"

    let t = mk_table_with(mk_compute(ir::Expr::Bool(false), ctxt), ctxt);

    let table_str = mk_compute(ir::Expr::Str("table".to_string()), ctxt);
    let function_str = mk_compute(ir::Expr::Str("function".to_string()), ctxt);
    let call_str = mk_compute(ir::Expr::Str("call".to_string()), ctxt);

    let ty = mk_compute(ir::Expr::Intrinsic(ir::Intrinsic::Type(arg)), ctxt);
    let is_table = mk_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, ty, table_str), ctxt);

    let arg_call = mk_node(ctxt);
    let ty_call = mk_node(ctxt);
    let ty_call_is_fn = mk_node(ctxt);

    let if_body = vec![
        ir::Statement::Compute(arg_call, ir::Expr::Index(arg, call_str)), // arg["call"]
        ir::Statement::Compute(ty_call, ir::Expr::Intrinsic(ir::Intrinsic::Type(arg_call))), // type(arg["call"])
        ir::Statement::Compute(ty_call_is_fn, ir::Expr::BinOp(ir::BinOpKind::IsEqual, ty_call, function_str)), // type(arg["call"]) == "function"
        ir::Statement::Store(t, ctxt.one, ty_call_is_fn), // t[1] = type(arg["call"]) == "function"
    ];
    push_st(ir::Statement::If(is_table, if_body, vec![]), ctxt);

    let bool_node = mk_compute(ir::Expr::Index(t, ctxt.one), ctxt); // return t[1]

    (bool_node, arg_call)
}

// check whether arg is a table, and not a function table!
fn mk_proper_table_check(arg: Node, ctxt: &mut Ctxt) -> Node {
    // return intrinsic::type(arg) == "table" && intrinsic::type(arg["call"]) != "function"
    let t = mk_table_with(mk_compute(ir::Expr::Bool(false), ctxt), ctxt);

    let table_str = mk_compute(ir::Expr::Str("table".to_string()), ctxt);
    let function_str = mk_compute(ir::Expr::Str("function".to_string()), ctxt);
    let call_str = mk_compute(ir::Expr::Str("call".to_string()), ctxt);

    let ty = mk_compute(ir::Expr::Intrinsic(ir::Intrinsic::Type(arg)), ctxt);
    let is_table = mk_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, ty, table_str), ctxt);

    let arg_call = mk_node(ctxt);
    let ty_call = mk_node(ctxt);
    let ty_call_is_fn = mk_node(ctxt);

    let if_body = vec![
        ir::Statement::Compute(arg_call, ir::Expr::Index(arg, call_str)), // arg["call"]
        ir::Statement::Compute(ty_call, ir::Expr::Intrinsic(ir::Intrinsic::Type(arg_call))), // type(arg["call"])
        ir::Statement::Compute(ty_call_is_fn, ir::Expr::BinOp(ir::BinOpKind::IsNotEqual, ty_call, function_str)), // type(arg["call"]) != "function"
        ir::Statement::Store(t, ctxt.one, ty_call_is_fn), // t[1] = type(arg["call"]) != "function"
    ];
    push_st(ir::Statement::If(is_table, if_body, vec![]), ctxt);

    let bool_node = mk_compute(ir::Expr::Index(t, ctxt.one), ctxt); // return t[1]

    bool_node
}

fn mk_assert(n: Node, s: &str, ctxt: &mut Ctxt) {
    let else_body = vec![
        ir::Statement::Compute(mk_node(ctxt), ir::Expr::Intrinsic(ir::Intrinsic::Throw(s.to_string())))
    ];
    push_st(ir::Statement::If(n, vec![], else_body), ctxt);
}

impl Ctxt {
    fn in_block(&mut self, f: impl FnOnce(&mut Ctxt)) -> Vec<ir::Statement> {
        self.locals.push(Default::default());
        let mut body = Vec::new();
        std::mem::swap(&mut self.body, &mut body);
        f(self);
        std::mem::swap(&mut self.body, &mut body);
        self.locals.pop().unwrap();

        body
    }
}

fn mk_num(x: impl Into<f64>, ctxt: &mut Ctxt) -> Node {
    let node = mk_node(ctxt);
    let expr = ir::Expr::Num(x.into());
    push_st(ir::Statement::Compute(node, expr), ctxt);

    node
}

fn mk_node(ctxt: &mut Ctxt) -> Node {
    let node = ctxt.next_node;
    ctxt.next_node += 1;

    node
}

fn mk_compute(expr: ir::Expr, ctxt: &mut Ctxt) -> Node {
    let node = mk_node(ctxt);
    push_st(ir::Statement::Compute(node, expr), ctxt);

    node
}

fn mk_table(ctxt: &mut Ctxt) -> Node {
    mk_compute(ir::Expr::NewTable, ctxt)
}

fn mk_table_with(val: Node, ctxt: &mut Ctxt) -> Node {
    let n = mk_table(ctxt);
    push_st(ir::Statement::Store(n, ctxt.one, val), ctxt);

    n
}

pub fn lower(ast: &Ast) -> IR {
    let mut ctxt = Ctxt::default();
    let (id, _) = lower_fn(&[], &Variadic::No, &ast.statements, /*is_main: */ true, &mut ctxt);

    let mut ir = ctxt.ir;
    ir.main_fn = id;

    ir
}

// should return a table from the expressions.
// table[0] should be the length of this table.
fn table_wrap_exprlist(exprs: &[Expr], start_node: Option<Node>, ctxt: &mut Ctxt) -> Node {
    let fields: Vec<_> = exprs.iter()
                      .cloned()
                      .map(Field::Expr)
                      .collect();

    lower_table(&fields, start_node, /*calc-length: */ true, ctxt)
}

fn push_st(st: ir::Statement, ctxt: &mut Ctxt) {
    ctxt.body.push(st);
}
