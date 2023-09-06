use crate::ir::{FnId, IR, Statement, Expr, BinOpKind, Intrinsic, BlockId, Node, Command};

use std::collections::HashMap;

type TablePtr = usize;

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

struct FnCtxt {
    arg: Value,
    nodes: HashMap<Node, Value>,
    fn_id: FnId,
    block_id: BlockId,
    statement_idx: usize,
}

struct Ctxt<'ir> {
    heap: Vec<TableData>,
    ir: &'ir IR,
    stack: Vec<FnCtxt>,
}

impl<'ir> Ctxt<'ir> { 
    fn fcx(&self) -> &FnCtxt {
        self.stack.last().unwrap()
    }

    fn fcx_mut(&mut self) -> &mut FnCtxt {
        self.stack.last_mut().unwrap()
    }
}

#[derive(Default)]
struct TableData {
    entries: Vec<(Value, Value)>,
    length: usize,
}

fn exec_command(cmd: &Command, ctxt: &mut Ctxt) {
    match cmd {
        Command::Print(n) => {
            let val = &ctxt.fcx().nodes[n];
            match val {
                Value::Nil => println!("nil"),
                Value::Bool(b) => println!("{}", b),
                Value::Str(s) => println!("{}", s),
                Value::TablePtr(ptr) => println!("table: {}", ptr),
                Value::LitFn(fid) => println!("function: {}", fid),
                Value::Num(x) => println!("{}", x),
            }
        }
        Command::Throw(s) => {
            // TODO only terminate this execution, not our whole Rust program.
            println!("ERROR: {}", s);
            std::process::exit(0);
        }
    }
}

fn exec_intrinsic(intrinsic: &Intrinsic, ctxt: &mut Ctxt) -> Value {
    match intrinsic {
        Intrinsic::Next(v1, v2) => {
            let v1 = &ctxt.fcx().nodes[v1];
            let Value::TablePtr(v1_) = v1 else { panic!("calling next onto non-table!") };

            let v2 = ctxt.fcx().nodes[v2].clone();

            table_next(*v1_, v2, ctxt)
        }
        Intrinsic::Type(n) => {
            let val = &ctxt.fcx().nodes[n];
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
    }
}

fn exec_expr(expr: &Expr, ctxt: &mut Ctxt) -> Value {
    match expr {
        Expr::Index(t, idx) => {
            let t = ctxt.fcx().nodes[t].clone();
            let idx = ctxt.fcx().nodes[idx].clone();

            let Value::TablePtr(t) = t else { panic!("indexing into non-table {:?}!", t) };
            table_get(t, idx, ctxt)
        },
        Expr::Arg => ctxt.fcx().arg.clone(),
        Expr::Intrinsic(intrinsic) => exec_intrinsic(intrinsic, ctxt),
        Expr::NewTable => Value::TablePtr(alloc_table(ctxt)),
        Expr::LitFunction(fnid) => Value::LitFn(*fnid),
        Expr::BinOp(kind, l, r) => {
            let l = ctxt.fcx().nodes[l].clone();
            let r = ctxt.fcx().nodes[r].clone();

            exec_binop(kind.clone(), l, r)
        },
        Expr::Len(r) => {
            let r = ctxt.fcx().nodes[r].clone();

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


fn call_fn(f: FnId, arg: Value, ctxt: &mut Ctxt) {
    let fcx = FnCtxt {
        nodes: Default::default(),
        arg,
        fn_id: f,
        block_id: ctxt.ir.fns[f].start_block,
        statement_idx: 0,
    };
    ctxt.stack.push(fcx);
}

fn alloc_table(ctxt: &mut Ctxt) -> TablePtr {
    let tid = ctxt.heap.len();
    ctxt.heap.push(Default::default());

    tid
}

pub fn exec(ir: &IR) {
    let mut ctxt = Ctxt {
        ir,
        heap: Vec::new(),
        stack: Vec::new(),
    };

    call_fn(ir.main_fn, Value::Nil, &mut ctxt);

    while ctxt.stack.len() > 0 {
        step(&mut ctxt);
    }
}

fn step_stmt(stmt: &Statement, ctxt: &mut Ctxt) {
    ctxt.fcx_mut().statement_idx += 1;

    use Statement::*;
    match stmt {
        Compute(n, expr) => {
            let val = exec_expr(expr, ctxt);
            ctxt.fcx_mut().nodes.insert(*n, val);
        }
        Store(t, idx, n) => {
            let t = ctxt.fcx().nodes[t].clone();
            let idx = ctxt.fcx().nodes[idx].clone();
            let val = ctxt.fcx().nodes[n].clone();
            let Value::TablePtr(t) = t.clone() else { panic!("indexing into non-table!") };
            table_set(t, idx, val, ctxt);
        },
        If(cond, then_body, else_body) => {
            let cond = ctxt.fcx().nodes[cond].clone();
            let blk = match &cond {
                Value::Bool(true) => then_body,
                Value::Bool(false) => else_body,
                _ => panic!("UB: non-boolean in if-condition"),
            };
            ctxt.fcx_mut().block_id = *blk;
            ctxt.fcx_mut().statement_idx = 0;
        },
        FnCall(f, arg) => {
            let f = ctxt.fcx().nodes[f].clone();
            let arg = ctxt.fcx().nodes[arg].clone();

            match f {
                Value::LitFn(f_id) => call_fn(f_id, arg, ctxt),
                v => panic!("trying to execute non-function value! {:?}", v),
            };
        },
        Command(cmd) => exec_command(cmd, ctxt),
        Return => {
            ctxt.stack.pop();
        },
    }
}

fn step(ctxt: &mut Ctxt) {
    let l: &FnCtxt = ctxt.stack.last().unwrap();
    let stmt = ctxt.ir.fns[l.fn_id].blocks[l.block_id].get(l.statement_idx).unwrap();
    step_stmt(stmt, ctxt);
}
