mod table;
use table::*;

mod native_fn;
use native_fn::*;

mod expr;
use expr::*;

mod fun;
use fun::*;

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

// returns the `Node` which stores the TablePtr to it.
fn locate_ident(s: &str, ctxt: &mut Ctxt) -> Node {
    for loc in ctxt.locals.iter().rev() {
        if let Some(n) = loc.get(s) {
            return *n;
        }
    }

    // if it's not defined in the locals, it has to be an upvalue!
    let new_n = mk_node(ctxt);
    ctxt.locals[0].insert(s.to_string(), new_n); // upvalues need to be on the bottom of the stack!
    ctxt.upvalue_idents.push(s.to_string());

    if ctxt.is_main { // or in-case of "main", just a local variable
        ctxt.body.insert(0, ir::Statement::Compute(new_n, ir::Expr::NewTable));
    } else {
        let (arg, upvalues_str, upvalues_table, upvalue_ident) = (mk_node(ctxt), mk_node(ctxt), mk_node(ctxt), mk_node(ctxt));
        ctxt.body.insert(0, ir::Statement::Compute(arg, ir::Expr::Arg));
        ctxt.body.insert(1, ir::Statement::Compute(upvalues_str, ir::Expr::Str("upvalues".to_string())));
        ctxt.body.insert(2, ir::Statement::Compute(upvalues_table, ir::Expr::Index(arg, upvalues_str)));
        ctxt.body.insert(3, ir::Statement::Compute(upvalue_ident, ir::Expr::Str(s.to_string())));
        ctxt.body.insert(4, ir::Statement::Compute(new_n, ir::Expr::Index(upvalues_table, upvalue_ident)));
    }

    new_n
}

fn lower_lvalue(lvalue: &LValue, ctxt: &mut Ctxt) -> (/*table: */ Node, /*index*/ Node) {
    match lvalue {
        LValue::Var(s) => {
            let n = locate_ident(s, ctxt);
            return (n, ctxt.one);
        },
        LValue::Dot(expr, field) => {
            let l = lower_expr1(expr, ctxt);
            let r = mk_compute(ir::Expr::Str(field.clone()), ctxt);
            mk_assert(mk_proper_table_check(l, ctxt), "Trying to index into non-table!", ctxt);

            (l, r)
        },
        LValue::Index(l, r) => {
            let l = lower_expr1(l, ctxt);
            let r = lower_expr1(r, ctxt);
            mk_assert(mk_proper_table_check(l, ctxt), "Trying to index into non-table!", ctxt);

            (l, r)
        },
    }
}

fn lower_assign(lvalues: &[(/*table: */ Node, /*index: */ Node)], exprs: &[Expr], ctxt: &mut Ctxt) {
    let mut exprs: Vec<Expr> = exprs.to_vec();

    // if exprs == [], just set everything to Nil!
    if exprs.is_empty() {
        let nil = mk_compute(ir::Expr::Nil, ctxt);
        for (t, idx) in lvalues.iter() {
            push_st(ir::Statement::Store(*t, *idx, nil), ctxt);
        }

        return;
    }

    // otherwise, last expr needs to handled differently.
    // if it's tabled, it needs to be unwrapped!
    let last = exprs.pop().unwrap();

    // non-tabled rhs nodes
    let mut rnodes: Vec<Node> = exprs.iter()
                     .map(|x| lower_expr1(x, ctxt))
                     .collect();
    let (last, tabled) = lower_expr(&last, ctxt);
    if !tabled {
        rnodes.push(last);
    }
    for ((t, i), r) in lvalues.iter().zip(rnodes.iter()) {
        push_st(ir::Statement::Store(*t, *i, *r), ctxt);
    }
    let min = rnodes.len();
    let max = lvalues.len();
    for i in min..max {
        let expr = if tabled {
            let i = i - min + 1; // starting at 1
            let x = mk_num(i as f64, ctxt);
            let x = ir::Expr::Index(last, x);

            x
        } else {
            ir::Expr::Nil
        };
        let r = mk_compute(expr, ctxt);
        let (t, idx) = lvalues[i].clone();
        push_st(ir::Statement::Store(t, idx, r), ctxt);
    }
}

// result is always tabled = true.
fn lower_fn_call(call: &FunctionCall, ctxt: &mut Ctxt) -> Node {
    let call_str = mk_compute(ir::Expr::Str(String::from("call")), ctxt);
    let upvalues_str = mk_compute(ir::Expr::Str(String::from("upvalues")), ctxt);
    let args_str = mk_compute(ir::Expr::Str(String::from("args")), ctxt);
    let retval_str = mk_compute(ir::Expr::Str(String::from("retval")), ctxt);

    match call {
        // f(x, y, z) --> f["call"]({"upvalues": f["upvalues"], "args": {[0]=3, x, y, z}})
        FunctionCall::Direct(f, args) => {
            let f = lower_expr1(f, ctxt);
            let f_call = mk_compute(ir::Expr::Index(f, call_str), ctxt);

            let arg = mk_table(ctxt);

            let args = table_wrap_exprlist(args, None, ctxt);
            push_st(ir::Statement::Store(arg, args_str, args), ctxt);

            let upvalues = mk_compute(ir::Expr::Index(f, upvalues_str), ctxt);
            push_st(ir::Statement::Store(arg, upvalues_str, upvalues), ctxt);

            push_st(ir::Statement::FnCall(f_call, arg), ctxt);

            mk_compute(ir::Expr::Index(arg, retval_str), ctxt)
        },
        // obj:f(x, y, z) --> t[idx]["call"]({"upvalues": t[idx]["upvalues"], "args": {[0]=4, obj, x, y, z}})
        FunctionCall::Colon(t, idx, args) => {
            let t = lower_expr1(t, ctxt);

            let idx = ir::Expr::Str(idx.clone());
            let idx = mk_compute(idx, ctxt);

            let f = mk_compute(ir::Expr::Index(t, idx), ctxt);
            let f_call = mk_compute(ir::Expr::Index(f, call_str), ctxt);

            let arg = mk_table(ctxt);

            let args = table_wrap_exprlist(args, Some(t), ctxt);
            push_st(ir::Statement::Store(arg, args_str, args), ctxt);

            let upvalues = mk_compute(ir::Expr::Index(f, upvalues_str), ctxt);
            push_st(ir::Statement::Store(arg, upvalues_str, upvalues), ctxt);

            push_st(ir::Statement::FnCall(f_call, arg), ctxt);
            mk_compute(ir::Expr::Index(arg, retval_str), ctxt)
        },
    }
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

fn lower_if(ifblocks: &[IfBlock], optelse: &Option<Vec<Statement>>, ctxt: &mut Ctxt) {
    assert!(ifblocks.len() > 0);

    let IfBlock(cond, ifbody) = ifblocks[0].clone();
    let cond = lower_expr1(&cond, ctxt);

    let ifbody = ctxt.in_block(|ctxt| {
        lower_body(&ifbody, ctxt);
    });

    let elsebody = ctxt.in_block(|ctxt| {
        if ifblocks.len() == 1 {
            if let Some(else_b) = optelse {
                lower_body(else_b, ctxt);
            }
        } else { // recursion!
            lower_if(&ifblocks[1..], optelse, ctxt);
        }
    });
    push_st(ir::Statement::If(cond, ifbody, elsebody), ctxt);
}

fn lower_body(statements: &[Statement], ctxt: &mut Ctxt) {
    for st in statements {
        match st {
            Statement::Assign(lvalues, exprs) => {
                let lvalues: Vec<(Node, Node)> = lvalues.iter()
                                             .map(|lval| lower_lvalue(lval, ctxt))
                                             .collect();
                lower_assign(&lvalues, exprs, ctxt);
            },
            Statement::Local(vars, exprs) => {
                let mut lvalues: Vec<(Node, Node)> = Vec::new();
                for _ in vars.iter() {
                    let n = mk_table(ctxt);
                    lvalues.push((n, ctxt.one));
                }
                lower_assign(&lvalues, exprs, ctxt);
                let map: &mut HashMap<_, _> = ctxt.locals.last_mut().unwrap();
                for (var, (n, _)) in vars.iter().zip(lvalues.iter()) {
                    map.insert(var.clone(), *n);
                }
            },
            Statement::FunctionCall(call) => { lower_fn_call(call, ctxt); },
            Statement::Return(exprs) => {
                let t = table_wrap_exprlist(exprs, None, ctxt);
                lower_return(t, ctxt);
            },
            Statement::Break => {
                push_st(ir::Statement::Break, ctxt);
            }
            Statement::While(cond, body) => {
                let body = ctxt.in_block(|ctxt| {
                    let cond = lower_expr1(cond, ctxt);
                    push_st(ir::Statement::If(cond, vec![], vec![ir::Statement::Break]), ctxt);

                    lower_body(body, ctxt);
                });

                push_st(ir::Statement::Loop(body), ctxt);
            },
            Statement::If(ifblocks, optelse) => { lower_if(ifblocks, optelse, ctxt); }
            Statement::Block(body) => {
                let body = ctxt.in_block(|ctxt| {
                    lower_body(body, ctxt);
                });
                ctxt.body.extend(body);
            },
            Statement::NumericFor(..) => unreachable!(),
            Statement::GenericFor(..) => unreachable!(),
            Statement::Repeat(..) => unreachable!(),
        }
    }
}

fn lower_return(/*the table we want to return*/ ret: Node, ctxt: &mut Ctxt) {
    if !ctxt.is_main {
        let retval_str = mk_compute(ir::Expr::Str("retval".to_string()), ctxt);
        let arg = mk_compute(ir::Expr::Arg, ctxt);
        push_st(ir::Statement::Store(arg, retval_str, ret), ctxt);
    }
    push_st(ir::Statement::Return, ctxt);
}

fn push_st(st: ir::Statement, ctxt: &mut Ctxt) {
    ctxt.body.push(st);
}
