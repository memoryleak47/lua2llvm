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
pub(self) use noisy_float::prelude::R64;

pub(self) use crate::ast::*;
pub(self) use crate::ir::{self, FnId, IR, Function, Node, BlockId};

struct FnCtxt {
    fn_id: FnId,
    
    // This Vec<> is pushed() & popped() for scopes, NOT functions.
    // The node `locals[ident]` contains a TablePtr with the single index 1.
    // upvalues are stored within here aswell, they store a table pointer to the variable in the stack frame where the upvalues originally came from.
    locals: Vec<HashMap<String, Node>>,

    // All idents that were used in this function, even though there were not locally defined.
    upvalue_idents: Vec<String>,

    next_node: Node,

    // Some(_) for variadics, None otherwise.
    ellipsis_node: Option<Node>,

    // `break_bid_stack.last().unwrap()` is where the `break` statement brings you.
    break_bid_stack: Vec<BlockId>,

    zero: Node,
    one: Node,
    true_: Node,
    retval_str: Node,
    call_str: Node,
    upvalues_str: Node,
    args_str: Node,
    table_str: Node,
    function_str: Node,
    count_str: Node,
    inner_str: Node,

    // typically block 0, used to initialize zero & one, and other things.
    init_block: Node,

    active_block: Option<BlockId>,
}

#[derive(Default)]
pub(self) struct Ctxt {
    ir: IR,
    fn_stack: Vec<FnCtxt>,
}

impl Ctxt {
    fn fcx(&self) -> &FnCtxt {
        self.fn_stack.last().unwrap()
    }

    fn fcx_mut(&mut self) -> &mut FnCtxt {
        self.fn_stack.last_mut().unwrap()
    }

    fn lit_fn(&self) -> &Function {
        &self.ir.fns[&self.fcx().fn_id]
    }

    fn lit_fn_mut(&mut self) -> &mut Function {
        let fid = self.fcx().fn_id;
        self.ir.fns.get_mut(&fid).unwrap()
    }

    fn is_main(&self) -> bool {
        self.fcx().fn_id == self.ir.main_fn
    }

    fn alloc_block(&mut self) -> BlockId {
        let fid = self.fcx().fn_id;
        let bid = self.ir.fns[&fid].blocks.len();
        self.ir.fns.get_mut(&fid).unwrap().blocks.insert(bid, Vec::new());

        bid
    }

    fn set_active_block(&mut self, block_id: BlockId) {
        self.fcx_mut().active_block = Some(block_id);
    }

    fn push_scope(&mut self) {
        self.fcx_mut().locals.push(Default::default());
    }

    fn pop_scope(&mut self) {
        self.fcx_mut().locals.pop().unwrap();
    }

    fn push_st(&mut self, st: ir::Statement) {
        if let Some(bid) = self.fcx().active_block {
            self.lit_fn_mut().blocks.get_mut(&bid).unwrap().push(st);
        }
    }

    fn push_compute(&mut self, expr: ir::Expr) -> Node {
        let node = mk_node(self);
        self.push_st(ir::Statement::Compute(node, expr));

        node
    }

    fn push_store(&mut self, table: Node, index: Node, val: Node) {
        self.push_st(ir::Statement::Store(table, index, val));
    }

    fn push_if(&mut self, cond: Node, then: BlockId, else_: BlockId) {
        self.push_st(ir::Statement::If(cond, then, else_));
        self.fcx_mut().active_block = None;
    }

    // like push_if, but `cond := truthy(cond)` is evaluated first.
    fn push_truthy_if(&mut self, cond: Node, then: BlockId, else_: BlockId) {
        let t = mk_table_with(self.true_(), self);

        let nil = self.push_compute(ir::Expr::Nil);
        let is_nil = self.push_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, cond, nil));
        let false_ = self.push_compute(ir::Expr::Bool(false));
        let is_false = self.push_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, cond, false_));

        let between_bid = self.alloc_block();
        let write_false_bid = self.alloc_block();
        let post_bid = self.alloc_block();

        self.push_if(is_nil, write_false_bid, between_bid);

        self.set_active_block(between_bid);
        self.push_if(is_false, write_false_bid, post_bid);

        self.set_active_block(write_false_bid);
        self.push_store(t, self.inner_str(), false_);
        self.push_goto(post_bid);

        self.set_active_block(post_bid);
        let cond = self.push_compute(ir::Expr::Index(t, self.inner_str()));

        self.push_if(cond, then, else_);
    }

    fn push_goto(&mut self, to: BlockId) {
        self.push_if(self.true_(), to, to);
    }

    fn zero(&self) -> Node { self.fcx().zero }
    fn one(&self) -> Node { self.fcx().one }
    fn true_(&self) -> Node { self.fcx().true_ }
    fn retval_str(&self) -> Node { self.fcx().retval_str }
    fn call_str(&self) -> Node { self.fcx().call_str }
    fn upvalues_str(&self) -> Node { self.fcx().upvalues_str }
    fn args_str(&self) -> Node { self.fcx().args_str }
    fn table_str(&self) -> Node { self.fcx().table_str }
    fn function_str(&self) -> Node { self.fcx().function_str }
    fn count_str(&self) -> Node { self.fcx().count_str }
    fn inner_str(&self) -> Node { self.fcx().inner_str }

    // add a few things to the init block.
    // This temporarily removes the `If` leading away from the init block.
    fn append_to_init_block<T>(&mut self, f: impl FnOnce(&mut Ctxt) -> T) -> T {
        let old_active = self.fcx_mut().active_block.clone();
        let init_bid = self.fcx().init_block;
        assert!(matches!(self.lit_fn().blocks[&init_bid].last(), Some(ir::Statement::If(_, _, _))));

        let final_if = self.lit_fn_mut().blocks.get_mut(&init_bid).unwrap().pop().unwrap();

        let t = f(self);

        self.lit_fn_mut().blocks.get_mut(&init_bid).unwrap().push(final_if);
        self.fcx_mut().active_block = old_active;

        t
    }
}


fn mk_num(x: impl Into<f64>, ctxt: &mut Ctxt) -> Node {
    let expr = ir::Expr::Num(R64::new(x.into()));
    ctxt.push_compute(expr)
}

fn mk_str(x: impl Into<String>, ctxt: &mut Ctxt) -> Node {
    let expr = ir::Expr::Str(x.into());
    ctxt.push_compute(expr)
}

fn mk_node(ctxt: &mut Ctxt) -> Node {
    let fcx = ctxt.fcx_mut();

    let node = fcx.next_node;
    fcx.next_node += 1;

    node
}

fn mk_table(ctxt: &mut Ctxt) -> Node {
    ctxt.push_compute(ir::Expr::NewTable)
}

fn mk_table_with(val: Node, ctxt: &mut Ctxt) -> Node {
    let n = mk_table(ctxt);
    ctxt.push_store(n, ctxt.inner_str(), val);

    n
}

pub fn lower(ast: &Ast) -> IR {
    let mut ctxt = Ctxt::default();
    lower_fn(&[], &Variadic::No, &ast.statements, /*is_main: */ true, &mut ctxt);

    ctxt.ir
}

// checks whether `arg` is a function, returns this in a new node as bool value.
fn mk_fn_check(arg: Node, ctxt: &mut Ctxt) -> (/*bool node*/ Node, /*call Node*/ Node) {
    // return Expr::Type(arg) == "table" && Expr::Type(arg["call"]) != "function"

    let t = mk_table_with(ctxt.push_compute(ir::Expr::Bool(false)), ctxt);

    let ty = ctxt.push_compute(ir::Expr::Type(arg));
    let is_table = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, ty, ctxt.table_str()));

    let then_body = ctxt.alloc_block();
    let post_body = ctxt.alloc_block();

    ctxt.push_if(is_table, then_body, post_body);

    ctxt.set_active_block(then_body);
    let arg_call = ctxt.push_compute(ir::Expr::Index(arg, ctxt.call_str()));  // arg["call"]
    let ty_call = ctxt.push_compute(ir::Expr::Type(arg_call));  // type(arg["call"])
    let ty_call_is_fn = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, ty_call, ctxt.function_str())); // type(arg["call"]) == "function"
    ctxt.push_store(t, ctxt.inner_str(), ty_call_is_fn); // t["inner"] = type(arg["call"]) == "function"
    ctxt.push_goto(post_body);

    ctxt.set_active_block(post_body);

    let bool_node = ctxt.push_compute(ir::Expr::Index(t, ctxt.inner_str())); // return t["inner"]

    (bool_node, arg_call)
}

// check whether arg is a table, and not a function table!
fn mk_proper_table_check(arg: Node, ctxt: &mut Ctxt) -> Node {
    // return Expr::Type(arg) == "table" && Expr::Type(arg["call"]) != "function"
    let t = mk_table_with(ctxt.push_compute(ir::Expr::Bool(false)), ctxt);

    let ty = ctxt.push_compute(ir::Expr::Type(arg));
    let is_table = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, ty, ctxt.table_str()));

    let then_body = ctxt.alloc_block();
    let post_body = ctxt.alloc_block();

    ctxt.push_if(is_table, then_body, post_body);

    ctxt.set_active_block(then_body);
    let arg_call = ctxt.push_compute(ir::Expr::Index(arg, ctxt.call_str()));  // arg["call"]
    let ty_call = ctxt.push_compute(ir::Expr::Type(arg_call));  // type(arg["call"])
    let ty_call_is_fn = ctxt.push_compute(ir::Expr::BinOp(ir::BinOpKind::IsNotEqual, ty_call, ctxt.function_str())); // type(arg["call"]) == "function"
    ctxt.push_store(t, ctxt.inner_str(), ty_call_is_fn); // t["inner"] = type(arg["call"]) == "function"
    ctxt.push_goto(post_body);

    ctxt.set_active_block(post_body);
    let bool_node = ctxt.push_compute(ir::Expr::Index(t, ctxt.inner_str())); // return t["inner"]

    bool_node
}

// `n` needs to be represent a boolean, otherwise this is UB.
fn mk_assert(n: Node, s: &str, ctxt: &mut Ctxt) {
    let else_body = ctxt.alloc_block();
    let post_body = ctxt.alloc_block();

    ctxt.push_if(n, post_body, else_body);

    ctxt.set_active_block(else_body);
    ctxt.push_st(ir::Statement::Throw(s.to_string()));

    ctxt.set_active_block(post_body);
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
