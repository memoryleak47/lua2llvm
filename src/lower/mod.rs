mod table;
use table::*;

mod native_fn;
use native_fn::*;

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

// same as lower_expr, but does _[1] for "tabled = true" automatically.
fn lower_expr1(expr: &Expr, ctxt: &mut Ctxt) -> Node {
    let (n, tabled) = lower_expr(expr, ctxt);
    if tabled {
        let x = ir::Expr::Index(n, mk_num(1.0, ctxt));
        let x = mk_compute(x, ctxt);

        x
    } else {
        n
    }
}

fn lower_binop(kind: &BinOpKind, l: &Expr, r: &Expr, ctxt: &mut Ctxt) -> Node {
    let kind = match kind {
        BinOpKind::Plus => ir::BinOpKind::Plus,
        BinOpKind::Minus => ir::BinOpKind::Minus,
        BinOpKind::Mul => ir::BinOpKind::Mul,
        BinOpKind::Div => ir::BinOpKind::Div,
        BinOpKind::Mod => ir::BinOpKind::Mod,
        BinOpKind::Lt => ir::BinOpKind::Lt,
        BinOpKind::Le => ir::BinOpKind::Le,
        BinOpKind::Gt => ir::BinOpKind::Gt,
        BinOpKind::Ge => ir::BinOpKind::Ge,
        BinOpKind::IsEqual => ir::BinOpKind::IsEqual,
        BinOpKind::IsNotEqual => ir::BinOpKind::IsNotEqual,
        BinOpKind::Concat => ir::BinOpKind::Concat,
        BinOpKind::Pow => ir::BinOpKind::Pow,

        BinOpKind::And => {
            let l: Node = lower_expr1(l, ctxt);
            let t = mk_table_with(l, ctxt);

            let if_body = ctxt.in_block(|ctxt| {
                let r: Node = lower_expr1(r, ctxt);
                push_st(ir::Statement::Store(t, ctxt.one, r), ctxt);
            });

            push_st(ir::Statement::If(l, if_body, vec![]), ctxt);

            return mk_compute(ir::Expr::Index(t, ctxt.one),  ctxt);
        },
        BinOpKind::Or => {
            let l: Node = lower_expr1(l, ctxt);
            let t = mk_table_with(l, ctxt);

            let else_body = ctxt.in_block(|ctxt| {
                let r: Node = lower_expr1(r, ctxt);
                push_st(ir::Statement::Store(t, ctxt.one, r), ctxt);
            });

            push_st(ir::Statement::If(l, vec![], else_body), ctxt);

            return mk_compute(ir::Expr::Index(t, ctxt.one), ctxt);
        },
    };

    let l = lower_expr1(l, ctxt);
    let r = lower_expr1(r, ctxt);
    let x = ir::Expr::BinOp(kind, l, r);

    mk_compute(x, ctxt)
}

fn lower_unop(kind: &UnOpKind, r: &Expr, ctxt: &mut Ctxt) -> Node {
    let r = lower_expr1(r, ctxt);

    let x = match kind {
        UnOpKind::Neg => ir::Expr::BinOp(ir::BinOpKind::Minus, ctxt.zero, r),
        UnOpKind::Len => ir::Expr::Len(r),
        UnOpKind::Not => {
            let true_v = mk_compute(ir::Expr::Bool(true), ctxt);
            let t = mk_table_with(true_v, ctxt);

            let if_body = ctxt.in_block(|ctxt| {
                let false_v = mk_compute(ir::Expr::Bool(false), ctxt);
                push_st(ir::Statement::Store(t, ctxt.one, false_v), ctxt);
            });

            push_st(ir::Statement::If(r, if_body, vec![]), ctxt);

            ir::Expr::Index(t, ctxt.one)
        },
    };

    mk_compute(x, ctxt)

}

// "tabled" is true for function calls and ellipsis expressions.
// which return tables after transforming them.
fn lower_expr(expr: &Expr, ctxt: &mut Ctxt) -> (Node, /*tabled: */ bool) {
    let mut tabled = false;
    let node = match expr {
        Expr::Ellipsis => {
            tabled = true;

            ctxt.ellipsis_node.expect("lowering `...` in non-variadic function!")
        },
        Expr::Literal(Literal::Function(args, variadic, body)) => {
            let call_str = mk_compute(ir::Expr::Str(String::from("call")), ctxt);
            let upvalues_str = mk_compute(ir::Expr::Str(String::from("upvalues")), ctxt);

            let (fid, upvalue_idents) = lower_fn(args, variadic, body, false, ctxt);

            let n = mk_table(ctxt);

            let call = mk_compute(ir::Expr::LitFunction(fid), ctxt);
            push_st(ir::Statement::Store(n, call_str, call), ctxt);

            let upvalues = mk_table(ctxt);
            for u in &upvalue_idents {
                let upvalue_ident = mk_compute(ir::Expr::Str(u.to_string()), ctxt);
                let n = locate_ident(u, ctxt);
                push_st(ir::Statement::Store(upvalues, upvalue_ident, n), ctxt);
            }
            push_st(ir::Statement::Store(n, upvalues_str, upvalues), ctxt);

            n
        },
        Expr::Literal(Literal::Table(fields)) => {
            lower_table(fields, None, /*calc-length: */ false, ctxt)
        },
        Expr::LValue(lval) => {
            let (t, idx) = lower_lvalue(lval, ctxt);
            let x = ir::Expr::Index(t, idx);

            mk_compute(x, ctxt)
        },
        Expr::BinOp(kind, l, r) => lower_binop(kind, l, r, ctxt),
        Expr::UnOp(kind, r) => lower_unop(kind, r, ctxt),
        Expr::FunctionCall(call) => {
            tabled = true;

            lower_fn_call(call, ctxt)
        },

        // literals
        Expr::Literal(Literal::Num(i)) => mk_num(*i as f64, ctxt),
        Expr::Literal(Literal::Bool(b)) => mk_compute(ir::Expr::Bool(*b), ctxt),
        Expr::Literal(Literal::Str(s)) => mk_compute(ir::Expr::Str(s.clone()), ctxt),
        Expr::Literal(Literal::Nil) => mk_compute(ir::Expr::Nil, ctxt),
    };

    (node, tabled)
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

            mk_compute(ir::Expr::FnCall(f_call, arg), ctxt)
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

            mk_compute(ir::Expr::FnCall(f_call, arg), ctxt)
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
                push_st(ir::Statement::Return(t), ctxt);
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

fn push_st(st: ir::Statement, ctxt: &mut Ctxt) {
    ctxt.body.push(st);
}

// creates a new function and readies the ctxt, such that one can start adding the function in the callback.
fn add_fn<T>(callback: impl FnOnce(&mut Ctxt) -> T, ctxt: &mut Ctxt) -> (FnId, T) {
    let fid = ctxt.ir.fns.len();

    // this dummy allows us to have a fixed id before lowering of this fn is done.
    // this is necessary eg. for closuring.
    let dummy_lit_fn = LitFunction {
        body: Vec::new(),
    };
    ctxt.ir.fns.push(dummy_lit_fn);

    // TODO the following vars are pretty much a whole Ctxt, why not simply create a Ctxt directly?
    let mut locals = vec![HashMap::new()];
    let mut body = Vec::new();
    let mut next_node = 0;
    let mut upvalue_idents: Vec<String> = Vec::new();
    let mut ellipsis_node = None;
    let mut zero = usize::MAX;
    let mut one = usize::MAX;
    let mut is_main = false;

    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.upvalue_idents, &mut upvalue_idents);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);
    std::mem::swap(&mut ctxt.zero, &mut zero);
    std::mem::swap(&mut ctxt.one, &mut one);
    std::mem::swap(&mut ctxt.is_main, &mut is_main);

    ctxt.zero = mk_num(0.0, ctxt);
    ctxt.one = mk_num(1.0, ctxt);

    let t = callback(ctxt);

    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.upvalue_idents, &mut upvalue_idents);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);
    std::mem::swap(&mut ctxt.zero, &mut zero);
    std::mem::swap(&mut ctxt.one, &mut one);
    std::mem::swap(&mut ctxt.is_main, &mut is_main);

    ctxt.ir.fns[fid] = LitFunction { body };

    (fid, t)
}

fn lower_fn(args: &[String], variadic: &Variadic, statements: &[Statement], is_main: bool, ctxt: &mut Ctxt) -> (FnId, Vec<String>) {
    add_fn(|ctxt| {
        ctxt.is_main = is_main;

        // global environment functions
        if is_main {
            add_native_fns(ctxt);
        }

        if !args.is_empty() || *variadic == Variadic::Yes {
            // function args
            let arg = mk_compute(ir::Expr::Arg, ctxt);
            let args_str = mk_compute(ir::Expr::Str("args".to_string()), ctxt);
            let argtable = mk_compute(ir::Expr::Index(arg, args_str), ctxt);

            for (i, arg) in args.iter().enumerate() {
                let t = mk_table(ctxt);
                // lua tables start with 1, not 0.
                let i = mk_num((i+1) as f64, ctxt);
                let val = mk_compute(ir::Expr::Index(argtable, i), ctxt);
                push_st(ir::Statement::Store(t, ctxt.one, val), ctxt);

                ctxt.locals.last_mut().unwrap().insert(arg.clone(), t);
            }

            if *variadic == Variadic::Yes {
                // ARG_LEN = args.len()
                // ARGT_LEN = argtable[0]
                // E_LEN = ARGT_LEN - ARG_LEN -- length of ellipsis `...`
                // n = {}
                // n[0] <- E_LEN
                // i = 1
                // loop {
                //   if i > E_LEN: break
                //   n[i] = argtable[i+ARG_LEN]
                //   i = i + 1
                // }
                let arg_len = mk_num(args.len() as f64, ctxt);
                let argt_len = mk_compute(ir::Expr::Index(argtable, ctxt.zero), ctxt);
                let e_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Minus, argt_len, arg_len), ctxt);
                let n = mk_table(ctxt);
                push_st(ir::Statement::Store(n, ctxt.zero, e_len), ctxt);

                let i = mk_table_with(ctxt.one, ctxt);

                let loopbody = ctxt.in_block(|ctxt| {
                    // if i > E_LEN: break
                    let i_node = mk_compute(ir::Expr::Index(i, ctxt.one), ctxt);
                    let i_gt_e_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Gt, i_node, e_len), ctxt);
                    push_st(ir::Statement::If(i_gt_e_len, vec![ir::Statement::Break], vec![]), ctxt);

                    // n[i] = argtable[i+ARG_LEN]
                    let i_plus_arg_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, arg_len), ctxt);
                    let argtable_indexed = mk_compute(ir::Expr::Index(argtable, i_plus_arg_len), ctxt);
                    push_st(ir::Statement::Store(n, i_node, argtable_indexed), ctxt);

                    // i = i+1
                    let i_plus_one = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, ctxt.one), ctxt);
                    push_st(ir::Statement::Store(i, ctxt.one, i_plus_one), ctxt);
                });
                push_st(ir::Statement::Loop(loopbody), ctxt);

                ctxt.ellipsis_node = Some(n);
            }
        }

        lower_body(statements, ctxt);

        // add `return` if missing
        if !matches!(ctxt.body.last(), Some(ir::Statement::Return(_))) {
            let t = mk_table(ctxt);
            push_st(ir::Statement::Store(t, ctxt.zero, ctxt.zero), ctxt);
            push_st(ir::Statement::Return(t), ctxt);
        }

        ctxt.upvalue_idents.clone()
    }, ctxt)
}
