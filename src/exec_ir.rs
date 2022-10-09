use crate::ir::{FnId, IR, Statement, Expr, LValue, BinOpKind, UnOpKind};

type TablePtr = usize;

type NativeFn = fn(TablePtr, &mut Ctxt) -> TablePtr;
type NativeFnId = usize;

static NATIVE_FNS: [NativeFn; 1] = [print_fn];

fn print_fn(t: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    match table_get(t, Value::Num(1.0), ctxt) {
        Value::Nil => println!("nil"),
        Value::Bool(b) => println!("{}", b),
        Value::Str(s) => println!("{}", s),
        Value::TablePtr(ptr) => println!("table {}", ptr),
        Value::NativeFn(_) => println!("<native-fn>"),
        Value::LuaFn(..) => println!("<lua-fn>"),
        Value::Num(x) => println!("{}", x),
    }

    alloc_table(ctxt)
}

fn table_get(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Value {
    ctxt.heap[ptr].entries.iter()
        .find(|(x, _)| *x == idx)
        .map(|(_, v)| v.clone())
        .unwrap_or(Value::Nil)
}

fn table_set(ptr: TablePtr, idx: Value, val: Value, ctxt: &mut Ctxt) {
    let entries = &mut ctxt.heap[ptr].entries;

    entries.retain(|(k, _)| *k != idx);
    if idx != Value::Nil {
        entries.push((idx, val));
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Value {
    Nil,
    Bool(bool),
    TablePtr(TablePtr),
    Str(String),
    NativeFn(NativeFnId),
    LuaFn(FnId),
    Num(f64),
}

struct Ctxt<'ir> {
    heap: Vec<TableData>,
    globals: Vec<Value>,
    ir: &'ir IR,

    // function-local:
    argtable: TablePtr,
    locals: Vec<Value>,
    nodes: Vec<Value>,
}

#[derive(Default)]
struct TableData {
    entries: Vec<(Value, Value)>,
    length: usize,
}

fn exec_expr(expr: &Expr, ctxt: &mut Ctxt) -> Value {
    match expr {
        Expr::LValue(LValue::Local(lid)) => ctxt.locals[*lid].clone(),
        Expr::LValue(LValue::Global(gid)) => ctxt.globals[*gid].clone(),
        Expr::LValue(LValue::Index(t, idx)) => {
            let t = ctxt.nodes[*t].clone();
            let idx = ctxt.nodes[*idx].clone();

            let Value::TablePtr(t) = t else { panic!("indexing into non-table!") };
            table_get(t, idx, ctxt)
        },
        Expr::LValue(_) => todo!(),
        Expr::Argtable => Value::TablePtr(ctxt.argtable),
        Expr::FnCall(f, argt) => {
            let f = ctxt.nodes[*f].clone();
            let argt = ctxt.nodes[*argt].clone();

            let Value::TablePtr(argt) = argt else { panic!("function called with non-table argtable!") };
            let retptr = match f {
                Value::LuaFn(f_id) => exec_fn(f_id, argt, ctxt),
                Value::NativeFn(i) => (NATIVE_FNS[i])(argt, ctxt),
                _ => panic!("trying to execute non-function value!"),
            };

            Value::TablePtr(retptr)
        },
        Expr::NewTable => Value::TablePtr(alloc_table(ctxt)),
        Expr::LitFunction(fnid) => Value::LuaFn(*fnid),
        Expr::BinOp(kind, l, r) => {
            let l = ctxt.nodes[*l].clone();
            let r = ctxt.nodes[*r].clone();

            exec_binop(kind.clone(), l, r)
        },
        Expr::UnOp(kind, r) => {
            let r = ctxt.nodes[*r].clone();

            exec_unop(kind.clone(), r, ctxt)
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


fn exec_unop(kind: UnOpKind, r: Value, ctxt: &Ctxt) -> Value {
    use UnOpKind::*;
    match (kind, r) {
        (Neg, Value::Num(x)) => Value::Num(-x),
        (Len, Value::TablePtr(ptr)) => Value::Num(ctxt.heap[ptr].length as f64),
        (Len, Value::Str(s)) => Value::Num(s.len() as f64),
        (Not, l) if truthy(&l) => Value::Bool(false),
        (Not, _) => Value::Bool(true),
        _ => panic!("type error!"),
    }
}

fn truthy(v: &Value) -> bool {
    !matches!(v, Value::Bool(false) | Value::Nil)
}

fn exec_fn(f: FnId, mut argtable: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    let mut locals = Vec::new();
    let mut nodes = Vec::new();

    let mut opt_retptr: Option<TablePtr> = None;

    std::mem::swap(&mut locals, &mut ctxt.locals);
    std::mem::swap(&mut nodes, &mut ctxt.nodes);
    std::mem::swap(&mut argtable, &mut ctxt.argtable);

    for st in &ctxt.ir.fns[f].body {
        use Statement::*;
        match st {
            Local(lid) => {
                while ctxt.locals.len() < lid+1 {
                    ctxt.locals.push(Value::Nil);
                }
                ctxt.locals[*lid] = Value::Nil;
            },
            Compute(n, expr) => {
                let val = exec_expr(expr, ctxt);
                while ctxt.nodes.len() < *n+1 {
                    ctxt.nodes.push(Value::Nil);
                }
                ctxt.nodes[*n] = val;
            }
            Store(lval, n) => {
                use LValue::*;
                let val = ctxt.nodes[*n].clone();
                match lval {
                    Local(lid) => { ctxt.locals[*lid] = val; },
                    Global(gid) => { ctxt.globals[*gid] = val; },
                    Index(t, idx) => {
                        let idx = ctxt.nodes[*idx].clone();
                        let Value::TablePtr(t) = ctxt.nodes[*t].clone() else { panic!("indexing into non-table!") };
                        table_set(t, idx, val, ctxt);
                    },
                    Upvalue(fnid, lid) => todo!(),
                }
            },
            ReturnTable(n) => todo!(),
            If(n, then, els) => todo!(),
            Loop(body) => todo!(),
            Break => todo!(),
        }
    }

    std::mem::swap(&mut locals, &mut ctxt.locals);
    std::mem::swap(&mut nodes, &mut ctxt.nodes);
    std::mem::swap(&mut argtable, &mut ctxt.argtable);

    if let Some(retptr) = opt_retptr {
        return retptr;
    } else {
        alloc_table(ctxt)
    }
}

fn alloc_table(ctxt: &mut Ctxt) -> TablePtr {
    let tid = ctxt.heap.len();
    ctxt.heap.push(Default::default());

    tid
}

pub fn exec(ir: &IR) {
    let mut ctxt = Ctxt {
        heap: Vec::new(),
        globals: vec![Value::Nil; ir.globals.len()],
        ir,
        argtable: 0,
        locals: Vec::new(),
        nodes: Vec::new(),
    };

    if let Some(i) = ir.globals.iter().position(|x| x == "print") {
        ctxt.globals[i] = Value::NativeFn(0);
    }

    let argtable = alloc_table(&mut ctxt);
    table_set(argtable, Value::Num(0.0), Value::Num(0.0), &mut ctxt);

    exec_fn(ir.main_fn, argtable, &mut ctxt);
}
