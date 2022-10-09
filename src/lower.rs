#![allow(unused)]

use std::collections::HashMap;

use crate::ast::*;
use crate::ir::{self, FnId, LocalId, GlobalId, IR, LitFunction, Node};

#[derive(Default)]
struct Ctxt {
    ir: IR,

    // the Vec<> is pushed() & popped() for blocks, NOT functions.
    locals: Vec<HashMap<String, LocalId>>,
    upvalues: HashMap<String, (FnId, LocalId)>,
    globals: HashMap<String, GlobalId>,

    // the fn whose body we are currently lowering.
    current_fn: FnId,

    // this is intended to be std::mem::swap'ped out, when needed.
    body: Vec<ir::Statement>,
    next_node: usize,
}

fn mk_compute(expr: ir::Expr, ctxt: &mut Ctxt) -> Node {
    let node = ctxt.next_node;
    ctxt.next_node += 1;

    push_st(ir::Statement::Compute(node, expr), ctxt);

    node
}

pub fn lower(ast: &Ast) -> IR {
    let mut ctxt = Ctxt::default();
    let id = lower_fn(&[], &Variadic::No, &ast.statements, &mut ctxt);

    let mut ir = ctxt.ir;
    ir.main_fn = id;

    ir
}

// same as lower_expr, but does _[1] for "tabled = true" automatically.
fn lower_expr1(expr: &Expr, ctxt: &mut Ctxt) -> Node {
    let (n, tabled) = lower_expr(expr, ctxt);
    if tabled {
        let x = mk_compute(ir::Expr::Num(1.0), ctxt);
        let x = ir::Expr::LValue(ir::LValue::Index(n, x));
        let x = mk_compute(x, ctxt);

        x
    } else {
        n
    }
}

// pushes expr to the table as the last element of a table constructor.
fn push_last_table_expr(t: Node, counter: usize, expr: &Expr, calc_length: bool, ctxt: &mut Ctxt) -> /*length-node: */ Option<Node> {
    let (val, tabled) = lower_expr(expr, ctxt);
    if tabled {
        // `orig_t_len = #t`
        let orig_t_len: Node = {
            let orig_len = counter-1;
            let orig_len = Literal::Num(orig_len as f64);
            let orig_len = Expr::Literal(orig_len);

            lower_expr1(&orig_len, ctxt)
        };

        // `len = val[0]`
        let len: Node = {
            let idx = Literal::Num(0.0);
            let idx = Expr::Literal(idx);
            let idx = lower_expr1(&idx, ctxt);

            let lval = ir::LValue::Index(val, idx);
            let lval = ir::Expr::LValue(lval);

            mk_compute(lval, ctxt)
        };

        // `local i`
        let i_var = mk_local(ctxt);
        let i_var: ir::LValue = ir::LValue::Local(i_var);

        // `i = 1`
        let one = Literal::Num(1.0);
        let one = Expr::Literal(one);
        let one = lower_expr1(&one, ctxt);
        push_st(ir::Statement::Store(i_var.clone(), one), ctxt);

        let mut loop_body = Vec::new();
        // `loop {`
        {
            // `if i > len: break`
            let i = ir::Expr::LValue(i_var.clone());
            let i = mk_compute(i, ctxt);
            let cond = ir::Expr::BinOp(ir::BinOpKind::Gt, i, len);
            let cond = mk_compute(cond, ctxt);
            let brk = ir::Statement::Break;
            let if_st = ir::Statement::If(cond, vec![brk], Vec::new());
            push_st(if_st, ctxt);

            // `t[i+orig_t_len] = val[i]`

            // `i = i + 1`
        }
        // `}`

        push_st(ir::Statement::Loop(loop_body), ctxt);

        if calc_length {
            // `outlength = i - 1 + orig_t_len`

            Some(todo!())
        } else { None }
    } else {
        let idx = Literal::Num(counter as f64);
        let idx = Expr::Literal(idx);
        let idx = lower_expr1(&idx, ctxt);

        if calc_length {
            let lval = ir::LValue::Index(t, idx);
            push_st(ir::Statement::Store(lval, val), ctxt);
            let node = mk_compute(ir::Expr::Num(counter as f64), ctxt);

            Some(node)
        } else {
            None
        }
    }
}

// will only calc length if it ends in a Field::Expr.
fn lower_table(fields: &[Field], calc_length: bool, ctxt: &mut Ctxt) -> (/*table: */ Node, /*length-node: */ Option<Node>) {
    let t = mk_compute(ir::Expr::NewTable, ctxt);

    let mut counter = 1; // the next Field::Expr id.
    for (i, f) in fields.iter().enumerate() {
        match f {
            Field::Expr(expr) => {
                if i == fields.len() - 1 {
                    let opt = push_last_table_expr(t, counter, expr, calc_length, ctxt);
                    return (t, opt);
                } else {
                    let idx = Literal::Num(counter as f64);
                    let idx = Expr::Literal(idx);
                    let idx = lower_expr1(&idx, ctxt);

                    counter += 1;
                    let lval = ir::LValue::Index(t, idx);
                    let val = lower_expr1(&expr, ctxt);
                    push_st(ir::Statement::Store(lval, val), ctxt);
                }
            },
            Field::NameToExpr(name, expr) => {
                let idx = Literal::Str(name.clone());
                let idx = Expr::Literal(idx);
                let idx = lower_expr1(&idx, ctxt);

                let val = lower_expr1(expr, ctxt);

                let lval = ir::LValue::Index(t, idx);
                push_st(ir::Statement::Store(lval, val), ctxt);
            },
            Field::ExprToExpr(idx, val) => {
                let idx = lower_expr1(idx, ctxt);
                let val = lower_expr1(val, ctxt);

                let lval = ir::LValue::Index(t, idx);
                push_st(ir::Statement::Store(lval, val), ctxt);
            },
        }
    }

    (t, None)
}

// "tabled" is true for function calls and ellipsis expressions.
// which return tables after transforming them.
fn lower_expr(expr: &Expr, ctxt: &mut Ctxt) -> (Node, /*tabled: */ bool) {
    let mut tabled = false;
    let node = match expr {
        Expr::Ellipsis => todo!(),
        Expr::Literal(Literal::Function(args, variadic, body)) => {
            let fid = lower_fn(args, variadic, body, ctxt);
            let x = ir::Expr::LitFunction(fid);

            mk_compute(x, ctxt)
        },
        Expr::Literal(Literal::Table(fields)) => {
            let (t, _) = lower_table(fields, /*calc-length: */ false, ctxt);

            t
        },
        Expr::LValue(lval) => {
            let x = lower_lvalue(lval, ctxt);
            let x = ir::Expr::LValue(x);

            mk_compute(x, ctxt)
        },
        Expr::BinOp(_kind, _l, _r) => todo!(),
        Expr::UnOp(_kind, _r) => todo!(),
        Expr::FunctionCall(call) => {
            tabled = true;

            lower_fn_call(call, ctxt)
        },

        // literals
        Expr::Literal(Literal::Num(i)) => mk_compute(ir::Expr::Num(*i), ctxt),
        Expr::Literal(Literal::Bool(b)) => mk_compute(ir::Expr::Bool(*b), ctxt),
        Expr::Literal(Literal::Str(s)) => mk_compute(ir::Expr::Str(s.clone()), ctxt),
        Expr::Literal(Literal::Nil) => mk_compute(ir::Expr::Nil, ctxt),
    };

    (node, tabled)
}

fn lower_lvalue(lvalue: &LValue, ctxt: &mut Ctxt) -> ir::LValue {
    match lvalue {
        LValue::Var(s) => {
            for loc in ctxt.locals.iter().rev() {
                if let Some(lid) = loc.get(s) {
                    return ir::LValue::Local(*lid);
                }
            }
            if let Some((fid, lid)) = ctxt.upvalues.get(s) {
                return ir::LValue::Upvalue(*fid, *lid);
            }
            if let Some(gid) = ctxt.globals.get(s) {
                return ir::LValue::Global(*gid);
            } else {
                let free_gid = match ctxt.globals.values().max() {
                    Some(max) => max+1,
                    None => 0,
                };
                ctxt.globals.insert(s.clone(), free_gid);
                return ir::LValue::Global(free_gid);
            }
        },
        LValue::Dot(expr, field) => {
            let l = lower_expr1(expr, ctxt);
            let field_expr = Expr::Literal(Literal::Str(field.clone()));
            let r = lower_expr1(&field_expr, ctxt);

            ir::LValue::Index(l, r)
        },
        LValue::Index(l, r) => {
            let l = lower_expr1(l, ctxt);
            let r = lower_expr1(r, ctxt);

            ir::LValue::Index(l, r)
        },
    }
}

// does not add the local to ctxt.locals!
fn mk_local(ctxt: &mut Ctxt) -> LocalId {
    let optmax: Option<LocalId> = ctxt.locals.iter()
                    .flat_map(|map| map.values())
                    .copied()
                    .max();
    let free_lid = match optmax {
        Some(max) => max+1,
        None => 0,
    };

    push_st(ir::Statement::Local(free_lid), ctxt);

    free_lid
}

fn lower_assign(lvalues: &[ir::LValue], exprs: &[Expr], ctxt: &mut Ctxt) {
    let mut exprs: Vec<Expr> = exprs.to_vec();
    let last = exprs.pop().unwrap();

    // non-tabled rhs nodes
    let mut rnodes: Vec<Node> = exprs.iter()
                     .map(|x| lower_expr1(x, ctxt))
                     .collect();
    let (last, tabled) = lower_expr(&last, ctxt);
    if !tabled {
        rnodes.push(last);
    }
    for (l, r) in lvalues.iter().zip(rnodes.iter()) {
        push_st(ir::Statement::Store(l.clone(), *r), ctxt);
    }
    let min = rnodes.len();
    let max = lvalues.len();
    for i in min..max {
        let expr = if tabled {
            let i = i - min + 1; // starting at 1
            let x = mk_compute(ir::Expr::Num(i as f64), ctxt);
            let x = ir::Expr::LValue(ir::LValue::Index(last, x));

            x
        } else {
            ir::Expr::Nil
        };
        let r = mk_compute(expr, ctxt);
        let l = lvalues[i].clone();
        push_st(ir::Statement::Store(l, r), ctxt);
    }
}

// result is always tabled = true.
fn lower_fn_call(call: &FunctionCall, ctxt: &mut Ctxt) -> Node {
    match call {
        FunctionCall::Direct(f, args) => {
            let f = lower_expr1(f, ctxt);
            let arg = table_wrap_exprlist(args, ctxt);

            mk_compute(ir::Expr::FnCall(f, arg), ctxt)
        },
        FunctionCall::Colon(t, idx, args) => {
            let t = lower_expr1(t, ctxt);

            let idx = ir::Expr::Str(idx.clone());
            let idx = mk_compute(idx, ctxt);

            let f = ir::LValue::Index(t, idx);
            let f = ir::Expr::LValue(f);
            let f = mk_compute(f, ctxt);

            let arg = table_wrap_exprlist(args, ctxt);

            mk_compute(ir::Expr::FnCall(f, arg), ctxt)
        },
    }
}

// should return a table from the expressions.
// table[0] should be the length of this table.
fn table_wrap_exprlist(exprs: &[Expr], ctxt: &mut Ctxt) -> Node {
    let fields: Vec<_> = exprs.iter()
                      .cloned()
                      .map(Field::Expr)
                      .collect();
    let (t, len) = lower_table(&fields, /*calc-length: */ true, ctxt);
    let len = len.unwrap();

    let idx = Literal::Num(0 as f64);
    let idx = Expr::Literal(idx);
    let idx = lower_expr1(&idx, ctxt);

    let lval = ir::LValue::Index(t, idx);
    push_st(ir::Statement::Store(lval, len), ctxt);

    t
}

fn lower_body(statements: &[Statement], ctxt: &mut Ctxt) {
    for st in statements {
        match st {
            Statement::Assign(lvalues, exprs) => {
                let lvalues: Vec<_> = lvalues.iter()
                                             .map(|lval| lower_lvalue(lval, ctxt))
                                             .collect();
                lower_assign(&lvalues, exprs, ctxt);
            },
            Statement::Local(vars, exprs) => {
                let mut lids: Vec<_> = vars.iter()
                                           .map(|x| mk_local(ctxt))
                                           .collect();
                let mut lvalues: Vec<_> = lids.iter()
                                              .copied()
                                              .map(ir::LValue::Local)
                                              .collect();
                lower_assign(&lvalues, exprs, ctxt);
                let mut map: &mut HashMap<_, _> = ctxt.locals.last_mut().unwrap();
                for (var, lid) in vars.iter().zip(lids.iter()) {
                    map.insert(var.clone(), *lid);
                }
            },
            Statement::FunctionCall(call) => { lower_fn_call(call, ctxt); },
            Statement::Return(exprs) => {
                let t = table_wrap_exprlist(exprs, ctxt);
                push_st(ir::Statement::ReturnTable(t), ctxt);
            },
            _ => todo!(),
    /*
            Statement::While(Expr, /*body: */ Vec<Statement>) => todo!(),
            Statement::Repeat(/*body: */ Vec<Statement>, Expr) => todo!(),
            Statement::NumericFor(/*ident: */String, /*start: */Expr, /*stop: */Expr, /*step: */Option<Expr>, /*body: */ Vec<Statement>) => todo!(),
            Statement::GenericFor(Vec<String>, Vec<Expr>, /*body: */ Vec<Statement>) => todo!(),
            Statement::If(Vec<IfBlock>, /*else-body: */ Option<Vec<Statement>>) => todo!(),
            Statement::Block(Vec<Statement>) => todo!(),
            Statement::Break => todo!(),
    */
        }
    }
}

fn push_st(st: ir::Statement, ctxt: &mut Ctxt) {
    ctxt.body.push(st);
}

fn lower_fn(args: &[String], variadic: &Variadic, statements: &[Statement], ctxt: &mut Ctxt) -> FnId {
    let fid = ctxt.ir.fns.len();

    // this dummy allows us to have a fixed id before lowering of this fn is done.
    // this is necessary eg. for closuring.
    let dummy_lit_fn = LitFunction { body: Vec::new(), };
    ctxt.ir.fns.push(dummy_lit_fn);

    let mut current_fn = fid;
    let mut locals = vec![HashMap::new()];
    let mut body = Vec::new();
    let mut next_node = 0;
    let mut upvalues = ctxt.upvalues.clone();
    for map in &ctxt.locals {
        for (var, lid) in map.iter() {
            upvalues.insert(var.clone(), (ctxt.current_fn, *lid));
        }
    }

    std::mem::swap(&mut ctxt.current_fn, &mut current_fn);
    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.upvalues, &mut upvalues);

    {
        let argtable = mk_compute(ir::Expr::Argtable, ctxt);

        for (i, arg) in args.iter().enumerate() {
            let lid = mk_local(ctxt);
            // lua tables start with 1, not 0.
            let i = i + 1;
            let i = ir::Expr::Num(i as f64);
            let i = mk_compute(i, ctxt);
            let idx = ir::LValue::Index(argtable, i);
            let idx = ir::Expr::LValue(idx);
            let node = mk_compute(idx, ctxt);
            let lvalue = ir::LValue::Local(lid);
            push_st(ir::Statement::Store(lvalue, node), ctxt);

            let mut map = ctxt.locals.last_mut().unwrap();
            map.insert(arg.clone(), lid);
        }

        if *variadic == Variadic::Yes {
            // TODO evaluate "..." from the rest of the argtable.
        }

        lower_body(statements, ctxt);
    }

    std::mem::swap(&mut ctxt.current_fn, &mut current_fn);
    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.upvalues, &mut upvalues);

    ctxt.ir.fns[fid] = LitFunction {
        body
    };

    fid
}
