use crate::ir::{FnId, IR, Statement, Expr, BinOpKind, Intrinsic};

type TablePtr = usize;

enum ControlFlow {
    Break,
    Return,
    End,
}

fn table_get(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Value {
    ctxt.heap[ptr].entries.iter()
        .find(|(x, _)| *x == idx)
        .map(|(_, v)| v.clone())
        .unwrap_or(Value::Nil)
}

fn table_set(ptr: TablePtr, idx: Value, val: Value, ctxt: &mut Ctxt) {
    if idx == Value::Nil {
        panic!("setting index with nil is forbidden in lua!");
    }

    let data: &mut TableData = ctxt.heap.get_mut(ptr).expect("table_set got dangling pointer!");
    data.entries.retain(|(x, _)| *x != idx);
    if val == Value::Nil { // Value::Nil means it's not there, so just don't add it!
        if idx == Value::Num((data.length) as f64) {
            // recalculate length (lua does it the same way)
            for i in 1.. {
                if data.entries.iter().any(|(x, _)| x == &Value::Num(i as f64)) {
                    data.length = i;
                } else { break; }
            }
        }
    } else {
        data.entries.push((idx.clone(), val));

        if idx == Value::Num((data.length+1) as f64) {
            // recalculate length
            for i in (data.length+1).. {
                if data.entries.iter().any(|(x, _)| x == &Value::Num(i as f64)) {
                    data.length = i;
                } else { break; }
            }
        }
    }
}

// this is not equivalent to luas "next", as it only returns the next key and not the value too.
fn table_next(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Value {
    let data = &ctxt.heap[ptr];
    if idx == Value::Nil {
        match data.entries.get(0) {
            Some((k, _)) => k.clone(),
            None => Value::Nil,
        }
    } else {
        let i = data.entries.iter().position(|(i, _)| *i == idx).expect("invalid key to next!");
        if let Some((k, _)) = data.entries.get(i+1) {
            k.clone()
        } else {
            Value::Nil
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Value {
    Nil,
    Bool(bool),
    TablePtr(TablePtr),
    Str(String),
    LitFn(FnId),
    Num(f64),
}

struct Ctxt<'ir> {
    heap: Vec<TableData>,
    ir: &'ir IR,

    // function-local:
    arg: Value,
    nodes: Vec<Value>,
}

#[derive(Default)]
struct TableData {
    entries: Vec<(Value, Value)>,
    length: usize,
}

fn exec_intrinsic(intrinsic: &Intrinsic, ctxt: &mut Ctxt) -> Value {
    match intrinsic {
        Intrinsic::Print(n) => {
            let val = &ctxt.nodes[*n];
            match val {
                Value::Nil => println!("nil"),
                Value::Bool(b) => println!("{}", b),
                Value::Str(s) => println!("{}", s),
                Value::TablePtr(ptr) => println!("table: {}", ptr),
                Value::LitFn(fid) => println!("function: {}", fid),
                Value::Num(x) => println!("{}", x),
            }

            Value::Nil
        }
        Intrinsic::Next(v1, v2) => {
            let v1 = &ctxt.nodes[*v1];
            let Value::TablePtr(v1_) = v1 else { panic!("calling next onto non-table!") };

            let v2 = ctxt.nodes[*v2].clone();

            table_next(*v1_, v2, ctxt)
        }
        Intrinsic::Type(n) => {
            let val = &ctxt.nodes[*n];
            let s = match val {
                Value::Nil => "nil",
                Value::Bool(_) => "boolean",
                Value::Str(_) => "string",
                Value::LitFn(..) => "function",
                Value::Num(_) => "number",
                Value::TablePtr(_) => "table"
            };

            Value::Str(s.to_string())
        }
        Intrinsic::Throw(s) => {
            // TODO only terminate this execution, not our whole Rust program.
            println!("ERROR: {}", s);
            std::process::exit(0);
        }
    }
}

fn exec_expr(expr: &Expr, ctxt: &mut Ctxt) -> Value {
    match expr {
        Expr::Index(t, idx) => {
            let t = ctxt.nodes[*t].clone();
            let idx = ctxt.nodes[*idx].clone();

            let Value::TablePtr(t) = t else { panic!("indexing into non-table {:?}!", t) };
            table_get(t, idx, ctxt)
        },
        Expr::Arg => ctxt.arg.clone(),
        Expr::Intrinsic(intrinsic) => exec_intrinsic(intrinsic, ctxt),
        Expr::NewTable => Value::TablePtr(alloc_table(ctxt)),
        Expr::LitFunction(fnid) => Value::LitFn(*fnid),
        Expr::BinOp(kind, l, r) => {
            let l = ctxt.nodes[*l].clone();
            let r = ctxt.nodes[*r].clone();

            exec_binop(kind.clone(), l, r)
        },
        Expr::Len(r) => {
            let r = ctxt.nodes[*r].clone();

            match r {
                Value::Str(s) => Value::Num(s.len() as f64),
                Value::TablePtr(t) => Value::Num(ctxt.heap[t].length as f64),
                _ => panic!("executing len on invalid type!"),
            }
        },
        Expr::Num(x) => Value::Num(*x),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Nil => Value::Nil,
        Expr::Str(s) => Value::Str(s.clone()),
    }
}

fn exec_binop(kind: BinOpKind, l: Value, r: Value) -> Value {
    use BinOpKind::*;

    match (kind, l, r) {
        (Plus, Value::Num(l), Value::Num(r)) => Value::Num(l + r),
        (Minus, Value::Num(l), Value::Num(r)) => Value::Num(l - r),
        (Mul, Value::Num(l), Value::Num(r)) => Value::Num(l * r),
        (Div, Value::Num(l), Value::Num(r)) => Value::Num(l / r),
        (Mod, Value::Num(l), Value::Num(r)) => Value::Num(l % r),
        (Pow, Value::Num(l), Value::Num(r)) => Value::Num(l.powf(r)),
        (Lt, Value::Num(l), Value::Num(r)) => Value::Bool(l < r),
        (Le, Value::Num(l), Value::Num(r)) => Value::Bool(l <= r),
        (Gt, Value::Num(l), Value::Num(r)) => Value::Bool(l > r),
        (Ge, Value::Num(l), Value::Num(r)) => Value::Bool(l >= r),
        (IsEqual, l, r) => Value::Bool(l == r),
        (IsNotEqual, l, r) => Value::Bool(l != r),
        (Concat, Value::Str(l), Value::Str(r)) => Value::Str(format!("{}{}", l, r)),
        _ => panic!("type error!"),
    }
}

fn truthy(v: &Value) -> bool {
    !matches!(v, Value::Bool(false) | Value::Nil)
}

fn exec_body(statements: &[Statement], ctxt: &mut Ctxt) -> ControlFlow {
    for st in statements {
        use Statement::*;
        match st {
            Compute(n, expr) => {
                let val = exec_expr(expr, ctxt);
                while ctxt.nodes.len() < *n+1 {
                    ctxt.nodes.push(Value::Nil);
                }
                ctxt.nodes[*n] = val;
            }
            Store(t, idx, n) => {
                let t = ctxt.nodes[*t].clone();
                let idx = ctxt.nodes[*idx].clone();
                let val = ctxt.nodes[*n].clone();
                let Value::TablePtr(t) = t.clone() else { panic!("indexing into non-table!") };
                table_set(t, idx, val, ctxt);
            },
            Return => return ControlFlow::Return,
            If(cond, then_body, else_body) => {
                let cond = ctxt.nodes[*cond].clone();
                if truthy(&cond) {
                    match exec_body(then_body, ctxt) {
                        ControlFlow::End => {},
                        x => return x,
                    }
                } else {
                    match exec_body(else_body, ctxt) {
                        ControlFlow::End => {},
                        x => return x,
                    }
                }
            },
            Loop(body) => {
                loop {
                    match exec_body(body, ctxt) {
                        ControlFlow::Break => break,
                        ControlFlow::End => {},
                        ControlFlow::Return => return ControlFlow::Return,
                    }
                }
            },

            FnCall(f, arg) => {
                let f = ctxt.nodes[*f].clone();
                let arg = ctxt.nodes[*arg].clone();

                match f {
                    Value::LitFn(f_id) => exec_fn(f_id, arg, ctxt),
                    v => panic!("trying to execute non-function value! {:?}", v),
                };
            },
            Break => return ControlFlow::Break,
        }
    }

    ControlFlow::End
}

fn exec_fn(f: FnId, arg: Value, ctxt: &mut Ctxt) {
    let mut nodes = Vec::new();
    let mut arg = arg;

    std::mem::swap(&mut nodes, &mut ctxt.nodes);
    std::mem::swap(&mut arg, &mut ctxt.arg);

    let flow = exec_body(&ctxt.ir.fns[f].body, ctxt);

    std::mem::swap(&mut nodes, &mut ctxt.nodes);
    std::mem::swap(&mut arg, &mut ctxt.arg);

    match flow {
        ControlFlow::Return => {},
        ControlFlow::Break => panic!("cannot break, if you are not in a loop!"),
        ControlFlow::End => panic!("function ended without returning!"),
    }
}

// returns a new table with only the special "length" field 0 set to 0, otherwise there is nothing.
fn empty(ctxt: &mut Ctxt) -> TablePtr {
    let t = alloc_table(ctxt);
    table_set(t, Value::Num(0.0), Value::Num(0.0), ctxt);

    t
}

fn alloc_table(ctxt: &mut Ctxt) -> TablePtr {
    let tid = ctxt.heap.len();
    ctxt.heap.push(Default::default());

    tid
}

pub fn exec(ir: &IR) {
    let mut ctxt = Ctxt {
        heap: Vec::new(),
        ir,
        arg: Value::Nil,
        nodes: Vec::new(),
    };

    // TODO do I need this?
    let arg = Value::TablePtr(empty(&mut ctxt));

    exec_fn(ir.main_fn, arg, &mut ctxt);
}
