#![allow(unused)]

use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::ir::{self, FnId, LocalId, GlobalId, IR, LitFunction, Node, UpvalueId};

#[derive(Default)]
struct Ctxt {
    ir: IR,

    // the Vec<> is pushed() & popped() for blocks, NOT functions.
    locals: Vec<HashMap<String, LocalId>>,

    // global variables
    globals: HashMap<String, GlobalId>,

    // the entries of this table were previously in upvalue_candidates and had at least one occurence.
    // UpvalueId is the resulting id within this function.
    upvalues: HashMap<String, UpvalueId>,

    // contains all variables, which are defined somewhere in a super-function scope.
    upvalue_candidates: HashSet<String>,

    next_local: usize,

    // the fn whose body we are currently lowering.
    current_fn: FnId,

    // this is intended to be std::mem::swap'ped out, when needed.
    body: Vec<ir::Statement>,
    next_node: usize,

    ellipsis_node: Option<Node>,
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
        let x = ir::Expr::LValue(ir::LValue::Index(n, mk_num(1.0, ctxt)));
        let x = mk_compute(x, ctxt);

        x
    } else {
        n
    }
}

// pushes expr to the table as the last element of a table constructor.
// counter == the next Field::Expr id.
fn push_last_table_expr(t: Node, counter: usize, expr: &Expr, calc_length: bool, ctxt: &mut Ctxt) {
    let (val, tabled) = lower_expr(expr, ctxt);
    if tabled {
        // `orig_t_len = #t`
        let orig_t_len = mk_num((counter-1) as f64, ctxt);

        // `len = val[0]`
        let len: Node = {
            let lval = ir::LValue::Index(val, mk_num(0.0, ctxt));
            let lval = ir::Expr::LValue(lval);

            mk_compute(lval, ctxt)
        };

        // `local i`
        let i_var = mk_local(ctxt);
        let i_var: ir::LValue = ir::LValue::Local(i_var);

        // `i = 1`
        let one = mk_num(1.0, ctxt);
        push_st(ir::Statement::Store(i_var.clone(), one), ctxt);

        let body = ctxt.in_block(|ctxt| {
            // `loop {`

            // `if i > len: break`
            let i = ir::Expr::LValue(i_var.clone());
            let i: Node = mk_compute(i, ctxt);
            let cond = ir::Expr::BinOp(ir::BinOpKind::Gt, i.clone(), len);
            let cond = mk_compute(cond, ctxt);
            let brk = ir::Statement::Break;
            let if_st = ir::Statement::If(cond, vec![brk], Vec::new());
            push_st(if_st, ctxt);

            // `t[i+orig_t_len] = val[i]`
            let r = ir::Expr::LValue(ir::LValue::Index(val, i.clone()));
            let r = mk_compute(r, ctxt);
            let idx = ir::Expr::BinOp(ir::BinOpKind::Plus, i, orig_t_len.clone());
            let idx = mk_compute(idx, ctxt);
            let lval = ir::LValue::Index(t, idx);
            let store = ir::Statement::Store(lval, r);
            push_st(store, ctxt);

            // `i = i + 1`
            let r = ir::Expr::BinOp(ir::BinOpKind::Plus, i, one);
            let r = mk_compute(r, ctxt);
            let store = ir::Statement::Store(i_var.clone(), r);
            push_st(store, ctxt);

            // `}`
        });

        push_st(ir::Statement::Loop(body), ctxt);

        if calc_length {
            // `outlength = i + (orig_t_len - 1)`
            let i = ir::Expr::LValue(i_var.clone());
            let i: Node = mk_compute(i, ctxt);

            let x = ir::Expr::BinOp(ir::BinOpKind::Plus, i, mk_num(counter as f64 - 2.0, ctxt));
            let x = mk_compute(x, ctxt);

            let zero = mk_compute(ir::Expr::Num(0.0), ctxt);
            push_st(ir::Statement::Store(ir::LValue::Index(t, zero), x), ctxt);
        }
    } else {
        let idx = mk_num(counter as f64, ctxt);
        let lval = ir::LValue::Index(t, idx);
        push_st(ir::Statement::Store(lval, val), ctxt);

        if calc_length {
            let len = idx; // length of array is the same as the highest index.
            let lval = ir::LValue::Index(t, mk_num(0.0, ctxt));
            push_st(ir::Statement::Store(lval, len), ctxt);
        }
    }
}

// if calc_length is true, the function assumes that only Field::Expr inputs are given.
// It then stores the length of the table in t[0].
fn lower_table(fields: &[Field], calc_length: bool, ctxt: &mut Ctxt) -> Node {
    let t = mk_compute(ir::Expr::NewTable, ctxt);

    if calc_length && fields.is_empty() {
        let zero = mk_num(0.0, ctxt);
        push_st(ir::Statement::Store(ir::LValue::Index(t, zero), zero), ctxt);
        return t;
    }

    let mut counter = 1; // the next Field::Expr id.
    for (i, f) in fields.iter().enumerate() {
        match f {
            Field::Expr(expr) => {
                if i == fields.len() - 1 {
                    push_last_table_expr(t, counter, expr, calc_length, ctxt);
                } else {
                    let idx = mk_num(counter as f64, ctxt);

                    counter += 1;
                    let lval = ir::LValue::Index(t, idx);
                    let val = lower_expr1(&expr, ctxt);
                    push_st(ir::Statement::Store(lval, val), ctxt);
                }
            },
            Field::NameToExpr(name, expr) => {
                assert_eq!(calc_length, false);

                let idx = ir::Expr::Str(name.clone());
                let idx = mk_compute(idx, ctxt);

                let val = lower_expr1(expr, ctxt);

                let lval = ir::LValue::Index(t, idx);
                push_st(ir::Statement::Store(lval, val), ctxt);
            },
            Field::ExprToExpr(idx, val) => {
                assert_eq!(calc_length, false);

                let idx = lower_expr1(idx, ctxt);
                let val = lower_expr1(val, ctxt);

                let lval = ir::LValue::Index(t, idx);
                push_st(ir::Statement::Store(lval, val), ctxt);
            },
        }
    }

    t
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
            let v = ir::LValue::Local(mk_local(ctxt));
            push_st(ir::Statement::Store(v.clone(), l), ctxt);

            let if_body = ctxt.in_block(|ctxt| {
                let r: Node = lower_expr1(r, ctxt);
                push_st(ir::Statement::Store(v.clone(), r), ctxt);
            });

            push_st(ir::Statement::If(l, if_body, Vec::new()), ctxt);

            return mk_compute(ir::Expr::LValue(v),  ctxt);
        },
        BinOpKind::Or => {
            let l: Node = lower_expr1(l, ctxt);
            let v = ir::LValue::Local(mk_local(ctxt));
            push_st(ir::Statement::Store(v.clone(), l), ctxt);

            let else_body = ctxt.in_block(|ctxt| {
                let r: Node = lower_expr1(r, ctxt);
                push_st(ir::Statement::Store(v.clone(), r), ctxt);
            });

            push_st(ir::Statement::If(l, Vec::new(), else_body), ctxt);

            return mk_compute(ir::Expr::LValue(v),  ctxt);
        },
    };

    let l = lower_expr1(l, ctxt);
    let r = lower_expr1(r, ctxt);
    let x = ir::Expr::BinOp(kind, l, r);

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
            let (fid, upnodes) = lower_fn(args, variadic, body, false, ctxt);
            let x = ir::Expr::LitFunction(fid, upnodes);

            mk_compute(x, ctxt)
        },
        Expr::Literal(Literal::Table(fields)) => {
            lower_table(fields, /*calc-length: */ false, ctxt)
        },
        Expr::LValue(lval) => {
            let x = lower_lvalue(lval, ctxt);
            let x = ir::Expr::LValue(x);

            mk_compute(x, ctxt)
        },
        Expr::BinOp(kind, l, r) => lower_binop(kind, l, r, ctxt),
        Expr::UnOp(kind, r) => {
            let x = lower_expr1(r, ctxt);
            let x = ir::Expr::UnOp(*kind, x);

            mk_compute(x, ctxt)
        },
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

fn lower_lvalue(lvalue: &LValue, ctxt: &mut Ctxt) -> ir::LValue {
    match lvalue {
        LValue::Var(s) => {
            for loc in ctxt.locals.iter().rev() {
                if let Some(lid) = loc.get(s) {
                    return ir::LValue::Local(*lid);
                }
            }
            if let Some(uid) = ctxt.upvalues.get(s) {
                let uid = *uid;
                let one = mk_num(1.0, ctxt);
                let upv = mk_compute(ir::Expr::Upvalue(uid), ctxt);
                return ir::LValue::Index(upv, one);
            }
            if ctxt.upvalue_candidates.contains(s) {
                let uid = ctxt.upvalues.len();
                ctxt.upvalues.insert(s.clone(), uid);

                // recursive call which will now match the if let Some() block above.
                return lower_lvalue(lvalue, ctxt);
            }
            if let Some(gid) = ctxt.globals.get(s) {
                return ir::LValue::Global(*gid);
            } else {
                let gid = ctxt.globals.len();
                ctxt.globals.insert(s.clone(), gid);
                return ir::LValue::Global(gid);
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
    let free_lid = ctxt.next_local;
    ctxt.next_local += 1;

    push_st(ir::Statement::Local(free_lid), ctxt);

    free_lid
}

fn lower_assign(lvalues: &[ir::LValue], exprs: &[Expr], ctxt: &mut Ctxt) {
    let mut exprs: Vec<Expr> = exprs.to_vec();

    // if exprs == [], just set everything to Nil!
    if exprs.is_empty() {
        let nil = mk_compute(ir::Expr::Nil, ctxt);
        for l in lvalues {
            push_st(ir::Statement::Store(l.clone(), nil), ctxt);
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
    for (l, r) in lvalues.iter().zip(rnodes.iter()) {
        push_st(ir::Statement::Store(l.clone(), *r), ctxt);
    }
    let min = rnodes.len();
    let max = lvalues.len();
    for i in min..max {
        let expr = if tabled {
            let i = i - min + 1; // starting at 1
            let x = mk_num(i as f64, ctxt);
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

    lower_table(&fields, /*calc-length: */ true, ctxt)
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
            Statement::Repeat(body, cond) => {
                let body = ctxt.in_block(|ctxt| {
                    lower_body(body, ctxt);

                    let cond = lower_expr1(cond, ctxt);
                    push_st(ir::Statement::If(cond, vec![ir::Statement::Break], vec![]), ctxt);

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
            Statement::NumericFor(ident, start, stop, optstep, body) => {
                let loc_var = ir::LValue::Local(mk_local(ctxt));

                let n_start = lower_expr1(start, ctxt);
                push_st(ir::Statement::Store(loc_var.clone(), n_start), ctxt);

                let n_stop = lower_expr1(stop, ctxt);
                let n_step = match optstep {
                    Some(x) => lower_expr1(x, ctxt),
                    None => mk_num(1.0, ctxt),
                };

                // loop:

                let b = ctxt.in_block(|ctxt| {
                    let n_var = mk_compute(ir::Expr::LValue(loc_var.clone()), ctxt);

                    // if step > 0
                    let zero = mk_num(0.0, ctxt);
                    let step_gt_0 = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Gt, n_step, zero), ctxt);

                    // if !(var <= limit): break
                    let ifblock = ctxt.in_block(|ctxt| {
                        let var_le_stop = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Le, n_var, n_stop), ctxt);
                        push_st(ir::Statement::If(var_le_stop, vec![], vec![ir::Statement::Break]), ctxt);
                    });

                    // else:
                    // if !(var >= limit): break
                    let elseblock = ctxt.in_block(|ctxt| {
                        let var_ge_stop = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Ge, n_var, n_stop), ctxt);
                        push_st(ir::Statement::If(var_ge_stop, vec![], vec![ir::Statement::Break]), ctxt);
                    });

                    // add this large if block!
                    push_st(ir::Statement::If(step_gt_0, ifblock, elseblock), ctxt);

                    // the actual body of the loop
                    let tmp = ctxt.in_block(|ctxt| {
                        // local v = var
                        let v = mk_local(ctxt);
                        let loc_v = ir::LValue::Local(v.clone());

                        ctxt.locals.last_mut()
                                   .unwrap()
                                   .insert(ident.clone(), v.clone());

                        push_st(ir::Statement::Store(loc_v, n_var), ctxt);

                        // block
                        lower_body(body, ctxt);
                    });
                    ctxt.body.extend(tmp);

                    // var = var + step
                    let n_var = mk_compute(ir::Expr::LValue(loc_var.clone()), ctxt);
                    let sum = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, n_var, n_step), ctxt);
                    push_st(ir::Statement::Store(loc_var, sum), ctxt);
                });
                push_st(ir::Statement::Loop(b), ctxt);
            },
            Statement::GenericFor(idents, exprs, body) => {
                let (f, s, var) = (mk_local(ctxt), mk_local(ctxt), mk_local(ctxt));
                let (f_lval, s_lval, var_lval) = (ir::LValue::Local(f), ir::LValue::Local(s), ir::LValue::Local(var));
                let lvals = vec![f_lval.clone(), s_lval.clone(), var_lval.clone()];
                lower_assign(&lvals, exprs, ctxt);

                let b = ctxt.in_block(|ctxt| {
                    let n_f = mk_compute(ir::Expr::LValue(f_lval.clone()), ctxt);
                    let n_s = mk_compute(ir::Expr::LValue(s_lval.clone()), ctxt);
                    let n_var = mk_compute(ir::Expr::LValue(var_lval.clone()), ctxt);

                    let t = mk_compute(ir::Expr::NewTable, ctxt);

                    let zero = mk_num(0.0, ctxt);
                    let one = mk_num(1.0, ctxt);
                    let two = mk_num(2.0, ctxt);

                    // t[0] = 2
                    push_st(ir::Statement::Store(ir::LValue::Index(t, zero), two), ctxt);

                    // t[1] = s
                    push_st(ir::Statement::Store(ir::LValue::Index(t, one), n_s), ctxt);

                    // t[2] = var
                    push_st(ir::Statement::Store(ir::LValue::Index(t, two), n_var), ctxt);

                    // call f(s, var) which is equivalent to f(t), where t = {s, var}
                    // rettable = f(s, var)
                    let rettable = mk_compute(ir::Expr::FnCall(n_f, t), ctxt);

                    for (i, ident) in idents.iter().enumerate() {
                        let local = mk_local(ctxt);
                        ctxt.locals.last_mut()
                                   .unwrap()
                                   .insert(ident.clone(), local);
                        let n_i = mk_num((i+1) as f64, ctxt);

                        // rettable_i = rettable[i]
                        let rettable_i = mk_compute(ir::Expr::LValue(ir::LValue::Index(rettable, n_i)), ctxt);
                        push_st(ir::Statement::Store(ir::LValue::Local(local), rettable_i), ctxt);

                        if i == 0 {
                            push_st(ir::Statement::Store(var_lval.clone(), rettable_i), ctxt);
                        }
                    }

                    let n_var = mk_compute(ir::Expr::LValue(var_lval.clone()), ctxt);
                    let n_nil = mk_compute(ir::Expr::Nil, ctxt);
                    let var_eq_nil = mk_compute(ir::Expr::BinOp(ir::BinOpKind::IsEqual, n_var, n_nil), ctxt);
                    push_st(ir::Statement::If(var_eq_nil, vec![ir::Statement::Break], vec![]), ctxt);

                    lower_body(body, ctxt);
                });
                push_st(ir::Statement::Loop(b), ctxt);
            },
        }
    }
}

fn push_st(st: ir::Statement, ctxt: &mut Ctxt) {
    ctxt.body.push(st);
}

fn lower_fn(args: &[String], variadic: &Variadic, statements: &[Statement], is_main: bool, ctxt: &mut Ctxt) -> (FnId, Vec<Node>) {
    let fid = ctxt.ir.fns.len();

    // this dummy allows us to have a fixed id before lowering of this fn is done.
    // this is necessary eg. for closuring.
    let dummy_lit_fn = LitFunction {
        body: Vec::new(),
    };
    ctxt.ir.fns.push(dummy_lit_fn);

    enum IncompleteUpvalueRef {
        Super, // to check which super-function variable this correesponds to check their strings!
        Local(LocalId),
        Upvalue(UpvalueId),
    }

    let mut current_fn = fid;
    let mut locals = vec![HashMap::new()];
    let mut body = Vec::new();
    let mut next_node = 0;
    let mut next_local = 0;
    let mut upvalues: HashMap<String, UpvalueId> = HashMap::new();
    let mut upvalue_candidates: HashSet<String>;

    // shows what the upvalue_candidates actually refer to in the parent frame.
    let mut upvalue_candidates_iref: HashMap<String, IncompleteUpvalueRef> = HashMap::new();
    let mut ellipsis_node = None;

    // fill upvalue_candidates, order of those is important:
    // local candidates overwrite upvalue candidates; inner local candidates overwrite outer local candidates!
    for s in ctxt.upvalue_candidates.iter() {
        upvalue_candidates_iref.insert(s.clone(), IncompleteUpvalueRef::Super);
    }
    for (s, uid) in ctxt.upvalues.iter() {
        upvalue_candidates_iref.insert(s.clone(), IncompleteUpvalueRef::Upvalue(*uid));
    }
    for loc in ctxt.locals.iter() {
        for (k, lid) in loc {
            upvalue_candidates_iref.insert(k.clone(), IncompleteUpvalueRef::Local(*lid));
        }
    }

    upvalue_candidates = upvalue_candidates_iref.keys().cloned().collect();

    std::mem::swap(&mut ctxt.current_fn, &mut current_fn);
    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.next_local, &mut next_local);
    std::mem::swap(&mut ctxt.upvalues, &mut upvalues);
    std::mem::swap(&mut ctxt.upvalue_candidates, &mut upvalue_candidates);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);

    {
        if is_main {
            let nfs = ["print", "type", "pairs", "next"];
            for f in nfs {
                let gid = ctxt.globals.len();
                ctxt.globals.insert(String::from(f), gid);
                let n = mk_compute(ir::Expr::NativeFn(String::from(f)), ctxt);
                push_st(ir::Statement::Store(ir::LValue::Global(gid), n), ctxt);
            }
        }

        let argtable = mk_compute(ir::Expr::Arg, ctxt);

        for (i, arg) in args.iter().enumerate() {
            let lid = mk_local(ctxt);
            // lua tables start with 1, not 0.
            let i = i + 1;
            let i = mk_num(i as f64, ctxt);
            let idx = ir::LValue::Index(argtable, i);
            let idx = ir::Expr::LValue(idx);
            let node = mk_compute(idx, ctxt);
            let lvalue = ir::LValue::Local(lid);
            push_st(ir::Statement::Store(lvalue, node), ctxt);

            let mut map = ctxt.locals.last_mut().unwrap();
            map.insert(arg.clone(), lid);
        }

        if *variadic == Variadic::Yes {
            let zero = mk_num(0.0, ctxt);
            let one = mk_num(1.0, ctxt);

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
            let argt_len = mk_compute(ir::Expr::LValue(ir::LValue::Index(argtable, zero)), ctxt);
            let e_len = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Minus, argt_len, arg_len), ctxt);
            let n = mk_compute(ir::Expr::NewTable, ctxt);
            push_st(ir::Statement::Store(ir::LValue::Index(n, zero), e_len), ctxt);

            let i = mk_local(ctxt);
            push_st(ir::Statement::Store(ir::LValue::Local(i), one), ctxt);

            let i_node = mk_node(ctxt);
            let i_gt_e_len = mk_node(ctxt);
            let i_plus_arg_len = mk_node(ctxt);
            let argtable_indexed = mk_node(ctxt); // argtable[i+ARGLEN]
            let i_plus_one = mk_node(ctxt);
            push_st(ir::Statement::Loop(vec![
                // if i > E_LEN: break
                ir::Statement::Compute(i_node, ir::Expr::LValue(ir::LValue::Local(i))),
                ir::Statement::Compute(i_gt_e_len, ir::Expr::BinOp(ir::BinOpKind::Gt, i_node, e_len)),
                ir::Statement::If(i_gt_e_len, vec![ir::Statement::Break], vec![]),

                // n[i] = argtable[i+ARG_LEN]
                ir::Statement::Compute(i_plus_arg_len, ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, arg_len)),
                ir::Statement::Compute(argtable_indexed, ir::Expr::LValue(ir::LValue::Index(argtable, i_plus_arg_len))),
                ir::Statement::Store(ir::LValue::Index(n, i_node), argtable_indexed),

                // i = i+1
                ir::Statement::Compute(i_plus_one, ir::Expr::BinOp(ir::BinOpKind::Plus, i_node, one)),
                ir::Statement::Store(ir::LValue::Local(i), i_plus_one),
            ]), ctxt);

            ctxt.ellipsis_node = Some(n);
        }

        lower_body(statements, ctxt);

        // add `return` if missing
        if !matches!(ctxt.body.last(), Some(ir::Statement::Return(_))) {
            let t = mk_compute(ir::Expr::NewTable, ctxt);
            let zero = mk_num(0.0, ctxt);
            push_st(ir::Statement::Store(ir::LValue::Index(t, zero), zero), ctxt);
            push_st(ir::Statement::Return(t), ctxt);
        }
    }

    std::mem::swap(&mut ctxt.current_fn, &mut current_fn);
    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.next_local, &mut next_local);
    std::mem::swap(&mut ctxt.upvalues, &mut upvalues);
    std::mem::swap(&mut ctxt.upvalue_candidates, &mut upvalue_candidates);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);

    let upvalue_nodes = {
        let mut ret = vec![usize::MAX; upvalues.len()];
        for (s, uid) in upvalues.iter() {
            let expr = match upvalue_candidates_iref[s] {
                IncompleteUpvalueRef::Super => {
                    let uid2 = ctxt.upvalues.len();
                    ctxt.upvalues.insert(s.clone(), uid2);

                    ir::Expr::Upvalue(uid2)
                },
                IncompleteUpvalueRef::Local(lid) => {
                    ir::Expr::LValue(ir::LValue::Local(lid))
                },
                IncompleteUpvalueRef::Upvalue(uid) => {
                    ir::Expr::Upvalue(uid)
                },
            };
            let n = mk_compute(expr, ctxt);
            ret[*uid] = n;
        }

        assert!(ret.iter().all(|x| x != &usize::MAX));

        ret
    };

    ctxt.ir.fns[fid] = LitFunction {
        body,
    };

    (fid, upvalue_nodes)
}
