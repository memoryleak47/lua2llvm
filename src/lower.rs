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

fn lower_expr(expr: &Expr, ctxt: &mut Ctxt) -> Node {
    match expr {
        Expr::Ellipsis => todo!(),
        Expr::Literal(Literal::Function(args, variadic, body)) => {
            let fid = lower_fn(args, variadic, body, ctxt);
            let x = ir::Expr::LitFunction(fid);

            mk_compute(x, ctxt)
        },
        Expr::Literal(Literal::Table(_fields)) => todo!(),
        Expr::LValue(lval) => {
            let x = lower_lvalue(lval, ctxt);
            let x = ir::Expr::LValue(x);

            mk_compute(x, ctxt)
        },
        Expr::BinOp(_kind, _l, _r) => todo!(),
        Expr::UnOp(_kind, _r) => todo!(),
        Expr::FunctionCall(_call) => todo!(),

        // literals
        Expr::Literal(Literal::Num(i)) => mk_compute(ir::Expr::Num(*i), ctxt),
        Expr::Literal(Literal::Bool(b)) => mk_compute(ir::Expr::Bool(*b), ctxt),
        Expr::Literal(Literal::Str(s)) => mk_compute(ir::Expr::Str(s.clone()), ctxt),
        Expr::Literal(Literal::Nil) => mk_compute(ir::Expr::Nil, ctxt),
    }
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
            let l = lower_expr(expr, ctxt);
            let field_expr = Expr::Literal(Literal::Str(field.clone()));
            let r = lower_expr(&field_expr, ctxt);

            ir::LValue::Index(l, r)
        },
        LValue::Index(l, r) => {
            let l = lower_expr(l, ctxt);
            let r = lower_expr(r, ctxt);

            ir::LValue::Index(l, r)
        },
    }
}

fn declare_local(name: String, ctxt: &mut Ctxt) -> LocalId {
    let optmax: Option<LocalId> = ctxt.locals.iter()
                    .flat_map(|map| map.values())
                    .copied()
                    .max();
    let free_lid = match optmax {
        Some(max) => max+1,
        None => 0,
    };
    ctxt.locals
        .last_mut()
        .unwrap()
        .insert(name, free_lid);

    push_st(ir::Statement::Local(free_lid), ctxt);

    free_lid
}

fn lower_body(statements: &[Statement], ctxt: &mut Ctxt) {
    for st in statements {
        match st {
            Statement::Assign(lvalues, exprs) => {
                todo!()
            },
            _ => todo!(),
    /*
            Statement::FunctionCall(FunctionCall) => todo!(),
            Statement::While(Expr, /*body: */ Vec<Statement>) => todo!(),
            Statement::Repeat(/*body: */ Vec<Statement>, Expr) => todo!(),
            Statement::NumericFor(/*ident: */String, /*start: */Expr, /*stop: */Expr, /*step: */Option<Expr>, /*body: */ Vec<Statement>) => todo!(),
            Statement::GenericFor(Vec<String>, Vec<Expr>, /*body: */ Vec<Statement>) => todo!(),
            Statement::If(Vec<IfBlock>, /*else-body: */ Option<Vec<Statement>>) => todo!(),
            Statement::Local(/*vars: */ Vec<String>, /*rhs: */ Vec<Expr>) => todo!(),
            Statement::Return(Vec<Expr>) => todo!(),
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
    let dummy_lit_fn = LitFunction { body: Vec::new(), local_count: 0 };
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
            let lid = declare_local(arg.clone(), ctxt);
            // lua tables start with 1, not 0.
            let i = i + 1;
            let i = ir::Expr::Num(i as f64);
            let i = mk_compute(i, ctxt);
            let idx = ir::LValue::Index(argtable, i);
            let idx = ir::Expr::LValue(idx);
            let node = mk_compute(idx, ctxt);
            let lvalue = ir::LValue::Local(lid);
            push_st(ir::Statement::Store(lvalue, node), ctxt);
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
        body,
        local_count: locals.len(),
    };

    fid
}
