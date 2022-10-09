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

    // this is intended to be std::mem::swap'ped out, when needed.
    current_body: Vec<ir::Statement>,
    node_count: usize,
}

fn mk_compute(expr: ir::Expr, ctxt: &mut Ctxt) -> Node {
    let node = ctxt.node_count;
    ctxt.node_count += 1;

    let st = ir::Statement::Compute(node, expr);
    ctxt.current_body.push(st);

    node
}

pub fn lower(ast: &Ast) -> IR {
    let mut ctxt = Ctxt::default();
    let id = lower_fn(&ast.statements, &mut ctxt);

    let mut ir = ctxt.ir;
    ir.main_fn = id;

    ir
}

fn lower_expr(expr: &Expr, ctxt: &mut Ctxt) -> Node {
    match expr {
        Expr::Ellipsis => todo!(),
        Expr::Literal(Literal::Function(_args, _variadic, _body)) => todo!(),
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

fn lower_fn(statements: &[Statement], ctxt: &mut Ctxt) -> FnId {
    let id = ctxt.ir.fns.len();

    // this dummy allows us to have a fixed id before lowering of this fn is done.
    // this is necessary eg. for closuring.
    let dummy_lit_fn = LitFunction { body: Vec::new(), local_count: 0 };
    ctxt.ir.fns.push(dummy_lit_fn);

    let mut body = Vec::new();
    std::mem::swap(&mut ctxt.current_body, &mut body);

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

    std::mem::swap(&mut ctxt.current_body, &mut body);

    ctxt.ir.fns[id] = LitFunction {
        body,
        local_count: 0,
    };

    id
}
