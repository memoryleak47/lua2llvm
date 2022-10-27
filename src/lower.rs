use std::collections::HashMap;

use crate::ast::*;
use crate::ir::{self, FnId, IR, LitFunction, Node, NATIVE_FNS};

#[derive(Default)]
struct Ctxt {
    ir: IR,

    // the Vec<> is pushed() & popped() for blocks, NOT functions.
    // The node `locals[ident]` contains a TablePtr with the single index 1.
    locals: Vec<HashMap<String, Node>>,

    // this is intended to be std::mem::swap'ped out, when needed.
    body: Vec<ir::Statement>,
    next_node: usize,

    // all identifiers which are not locals end up in here.
    // this will result in either a local variable for main, or an Upvalue for non-main.
    // The corresponding `Node` needs to be initialized with NewTable at function start.
    // This `Node` is a table, which needs to be indexed at 1 to obtain the actual value.
    unknown_idents: HashMap<String, Node>,

    // Some(_) for variadics, None otherwise.
    ellipsis_node: Option<Node>,

    zero: Node,
    one: Node,
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

// pushes expr to the table as the last element of a table constructor.
// counter == the next Field::Expr id.
fn push_last_table_expr(t: Node, counter: usize, expr: &Expr, calc_length: bool, ctxt: &mut Ctxt) {
    let (val, tabled) = lower_expr(expr, ctxt);
    if tabled {
        // `orig_t_len = #t`
        let orig_t_len = mk_num((counter-1) as f64, ctxt);

        // `len = val[0]`
        let len = mk_compute(ir::Expr::Index(val, ctxt.zero), ctxt);

        // `local i = 1`
        let i_var = mk_table_with(ctxt.one, ctxt);

        let body = ctxt.in_block(|ctxt| {
            // `loop {`

            // `if i > len: break`
            let i = mk_compute(ir::Expr::Index(i_var, ctxt.one), ctxt);
            let cond = ir::Expr::BinOp(ir::BinOpKind::Gt, i, len);
            let cond = mk_compute(cond, ctxt);
            let brk = ir::Statement::Break;
            push_st(ir::Statement::If(cond, vec![brk], Vec::new()), ctxt);

            // `t[i+orig_t_len] = val[i]`
            let r = mk_compute(ir::Expr::Index(val, i), ctxt);
            let idx = ir::Expr::BinOp(ir::BinOpKind::Plus, i, orig_t_len.clone());
            let idx = mk_compute(idx, ctxt);
            let store = ir::Statement::Store(t, idx, r);
            push_st(store, ctxt);

            // `i = i + 1`
            let r = ir::Expr::BinOp(ir::BinOpKind::Plus, i, ctxt.one);
            let r = mk_compute(r, ctxt);
            let store = ir::Statement::Store(i_var, ctxt.one, r);
            push_st(store, ctxt);

            // `}`
        });

        push_st(ir::Statement::Loop(body), ctxt);

        if calc_length {
            // `outlength = i + (orig_t_len - 1)`
            let i = ir::Expr::Index(i_var, ctxt.one);
            let i = mk_compute(i, ctxt);

            let x = ir::Expr::BinOp(ir::BinOpKind::Plus, i, mk_num(counter as f64 - 2.0, ctxt));
            let x = mk_compute(x, ctxt);

            push_st(ir::Statement::Store(t, ctxt.zero, x), ctxt);
        }
    } else {
        let idx = mk_num(counter as f64, ctxt);
        push_st(ir::Statement::Store(t, idx, val), ctxt);

        if calc_length {
            let len = idx; // length of array is the same as the highest index.
            push_st(ir::Statement::Store(t, ctxt.zero, len), ctxt);
        }
    }
}

// if calc_length is true, the function assumes that only Field::Expr inputs are given.
// It then stores the length of the table in t[0].
fn lower_table(fields: &[Field], calc_length: bool, ctxt: &mut Ctxt) -> Node {
    let t = mk_table(ctxt);

    if calc_length && fields.is_empty() {
        push_st(ir::Statement::Store(t, ctxt.zero, ctxt.zero), ctxt);
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
                    let val = lower_expr1(&expr, ctxt);
                    push_st(ir::Statement::Store(t, idx, val), ctxt);
                }
            },
            Field::NameToExpr(name, expr) => {
                assert_eq!(calc_length, false);

                let idx = ir::Expr::Str(name.clone());
                let idx = mk_compute(idx, ctxt);

                let val = lower_expr1(expr, ctxt);

                push_st(ir::Statement::Store(t, idx, val), ctxt);
            },
            Field::ExprToExpr(idx, val) => {
                assert_eq!(calc_length, false);

                let idx = lower_expr1(idx, ctxt);
                let val = lower_expr1(val, ctxt);

                push_st(ir::Statement::Store(t, idx, val), ctxt);
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
            let (fid, upnodes) = lower_fn(args, variadic, body, false, ctxt);
            let x = ir::Expr::LitFunction(fid, upnodes);

            mk_compute(x, ctxt)
        },
        Expr::Literal(Literal::Table(fields)) => {
            lower_table(fields, /*calc-length: */ false, ctxt)
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
    ctxt.unknown_idents.get(s).cloned().unwrap_or_else(|| {
        let new_n = mk_node(ctxt);
        ctxt.unknown_idents.insert(s.to_string(), new_n);

        new_n
    })
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

            (l, r)
        },
        LValue::Index(l, r) => {
            let l = lower_expr1(l, ctxt);
            let r = lower_expr1(r, ctxt);

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

            let f = mk_compute(ir::Expr::Index(t, idx), ctxt);

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
                let loc_var = mk_table(ctxt);
                let n_start = lower_expr1(start, ctxt);
                push_st(ir::Statement::Store(loc_var, ctxt.one, n_start), ctxt);

                let n_stop = lower_expr1(stop, ctxt);
                let n_step = match optstep {
                    Some(x) => lower_expr1(x, ctxt),
                    None => ctxt.one,
                };

                // loop:

                let b = ctxt.in_block(|ctxt| {
                    let n_var = mk_compute(ir::Expr::Index(loc_var, ctxt.one), ctxt);

                    // if step > 0
                    let step_gt_0 = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Gt, n_step, ctxt.zero), ctxt);

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
                        let v = mk_table(ctxt);

                        ctxt.locals.last_mut()
                                   .unwrap()
                                   .insert(ident.clone(), v);

                        push_st(ir::Statement::Store(v, ctxt.one, n_var), ctxt);

                        // block
                        lower_body(body, ctxt);
                    });
                    ctxt.body.extend(tmp);

                    // var = var + step
                    let n_var = mk_compute(ir::Expr::Index(loc_var, ctxt.one), ctxt);
                    let sum = mk_compute(ir::Expr::BinOp(ir::BinOpKind::Plus, n_var, n_step), ctxt);
                    push_st(ir::Statement::Store(loc_var, ctxt.one, sum), ctxt);
                });
                push_st(ir::Statement::Loop(b), ctxt);
            },
            Statement::GenericFor(idents, exprs, body) => {
                let (f, s, var) = (mk_table(ctxt), mk_table(ctxt), mk_table(ctxt));
                let lvals = vec![(f, ctxt.one), (s, ctxt.one), (var, ctxt.one)];
                lower_assign(&lvals, exprs, ctxt);

                let b = ctxt.in_block(|ctxt| {
                    let n_f = mk_compute(ir::Expr::Index(f, ctxt.one), ctxt);
                    let n_s = mk_compute(ir::Expr::Index(s, ctxt.one), ctxt);
                    let n_var = mk_compute(ir::Expr::Index(var, ctxt.one), ctxt);

                    let t = mk_table(ctxt);

                    let two = mk_num(2.0, ctxt);

                    // t[0] = 2
                    push_st(ir::Statement::Store(t, ctxt.zero, two), ctxt);

                    // t[1] = s
                    push_st(ir::Statement::Store(t, ctxt.one, n_s), ctxt);

                    // t[2] = var
                    push_st(ir::Statement::Store(t, two, n_var), ctxt);

                    // call f(s, var) which is equivalent to f(t), where t = {s, var}
                    // rettable = f(s, var)
                    let rettable = mk_compute(ir::Expr::FnCall(n_f, t), ctxt);

                    for (i, ident) in idents.iter().enumerate() {
                        let local = mk_table(ctxt);
                        ctxt.locals.last_mut()
                                   .unwrap()
                                   .insert(ident.clone(), local);
                        let n_i = mk_num((i+1) as f64, ctxt);

                        // rettable_i = rettable[i]
                        let rettable_i = mk_compute(ir::Expr::Index(rettable, n_i), ctxt);
                        push_st(ir::Statement::Store(local, ctxt.one, rettable_i), ctxt);

                        if i == 0 {
                            push_st(ir::Statement::Store(var, ctxt.one, rettable_i), ctxt);
                        }
                    }

                    let n_var = mk_compute(ir::Expr::Index(var, ctxt.one), ctxt);
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

    let mut locals = vec![HashMap::new()];
    let mut body = Vec::new();
    let mut next_node = 0;
    let mut unknown_idents: HashMap<String, Node> = HashMap::new();
    let mut ellipsis_node = None;
    let mut zero = usize::MAX;
    let mut one = usize::MAX;

    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.unknown_idents, &mut unknown_idents);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);
    std::mem::swap(&mut ctxt.zero, &mut zero);
    std::mem::swap(&mut ctxt.one, &mut one);

    {
        ctxt.zero = mk_num(0.0, ctxt);
        ctxt.one = mk_num(1.0, ctxt);
        
        // global environment functions
        if is_main {
            for (i, f) in NATIVE_FNS.iter().enumerate() {
                let n = mk_compute(ir::Expr::NativeFn(i), ctxt);
                let t = mk_table_with(n, ctxt);
                ctxt.locals.last_mut().unwrap().insert(String::from(*f), t);
            }
        }

        if !args.is_empty() || *variadic == Variadic::Yes {
            // function args
            let argtable = mk_compute(ir::Expr::Arg, ctxt);

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
    }

    std::mem::swap(&mut ctxt.locals, &mut locals);
    std::mem::swap(&mut ctxt.body, &mut body);
    std::mem::swap(&mut ctxt.next_node, &mut next_node);
    std::mem::swap(&mut ctxt.unknown_idents, &mut unknown_idents);
    std::mem::swap(&mut ctxt.ellipsis_node, &mut ellipsis_node);
    std::mem::swap(&mut ctxt.zero, &mut zero);
    std::mem::swap(&mut ctxt.one, &mut one);

    // initialize unknown_idents
    let mut upvalue_nodes = Vec::new();
    if is_main {
        for (_, node) in unknown_idents {
            body.insert(0, ir::Statement::Compute(node, ir::Expr::NewTable));
        }
    } else {
        for (uid, (ident, node)) in unknown_idents.iter().enumerate() {
            body.insert(0, ir::Statement::Compute(*node, ir::Expr::Upvalue(uid)));
            let t = locate_ident(ident, ctxt);
            upvalue_nodes.push(t);
        }
    }

    ctxt.ir.fns[fid] = LitFunction { body };

    (fid, upvalue_nodes)
}
