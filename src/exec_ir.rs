use crate::ir::{FnId, IR, Statement, Expr, LValue, BinOpKind, UnOpKind, UpvalueRef};

type TablePtr = usize;

type NativeFn = fn(TablePtr, &mut Ctxt) -> TablePtr;
type NativeFnId = usize;

static NATIVE_FNS: [NativeFn; 4] = [print_fn, next_fn, pairs_fn, type_fn];

fn print_fn(t: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    let Value::Num(count) = table_get(t, Value::Num(0.0), ctxt) else { panic!("non-numeric table[0]!") };
    let count = count as usize;

    for i in 0..count {
        let v = table_get(t, Value::Num((i+1) as f64), ctxt);
        match v {
            Value::Nil => print!("nil"),
            Value::Bool(b) => print!("{}", b),
            Value::Str(s) => print!("{}", s),
            Value::TablePtr(ptr) => print!("table {}", ptr),
            Value::NativeFn(_) => print!("<native-fn>"),
            Value::LuaFn(..) => print!("<lua-fn>"),
            Value::Num(x) => print!("{}", x),
        }
        if i == count-1 {
            println!();
        } else {
            print!("    ");
        }
    }

    empty(ctxt)
}

fn next_fn(t: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    let Value::TablePtr(tt) = table_get(t, Value::Num(1.0), ctxt) else { panic!("calling next on non-table!") };
    let r = table_get(t, Value::Num(2.0), ctxt);

    wrap_vec(table_next(tt, r, ctxt), ctxt)
}

fn pairs_fn(t: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    let arg = table_get(t, Value::Num(1.0), ctxt);

    wrap_vec(vec![Value::NativeFn(1), arg, Value::Nil], ctxt)
}

fn type_fn(t: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    let arg = table_get(t, Value::Num(1.0), ctxt);

    let string = match arg {
        Value::Nil => "nil",
        Value::Bool(_) => "boolean",
        Value::TablePtr(_) => "table",
        Value::Str(_) => "string",
        Value::NativeFn(_) => "function",
        Value::LuaFn(..) => "function",
        Value::Num(_) => "number",
    };

    wrap_vec(vec![Value::Str(string.to_string())], ctxt)
}

enum ControlFlow {
    Break,
    ReturnTable(TablePtr),
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


fn table_next(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Vec<Value> {
    let data = &ctxt.heap[ptr];
    if idx == Value::Nil {
        match data.entries.get(0) {
            Some((k, v)) => vec![k.clone(), v.clone()],
            None => Vec::new(),
        }
    } else {
        let i = data.entries.iter().position(|(i, _)| *i == idx).expect("invalid key to next!");
        if let Some((k, v)) = data.entries.get(i+1) {
            vec![k.clone(), v.clone()]
        } else {
            Vec::new()
        }
    }
}

fn wrap_vec(vec: Vec<Value>, ctxt: &mut Ctxt) -> TablePtr {
    let t = alloc_table(ctxt);
    let len = vec.len();
    table_set(t, Value::Num(0.0), Value::Num(len as f64), ctxt);
    for (i, v) in vec.into_iter().enumerate() {
        let i = Value::Num((i+1) as f64);
        table_set(t, i, v, ctxt);
    }

    t
}

#[derive(Clone, PartialEq, Debug)]
enum Value {
    Nil,
    Bool(bool),
    TablePtr(TablePtr),
    Str(String),
    NativeFn(NativeFnId),
    LuaFn(FnId, /*upvalues: */ Vec<Value>),
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
    upvalues: Vec<Value>,
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
        Expr::Upvalue(uid) => ctxt.upvalues[*uid].clone(),
        Expr::Argtable => Value::TablePtr(ctxt.argtable),
        Expr::FnCall(f, argt) => {
            let f = ctxt.nodes[*f].clone();
            let argt = ctxt.nodes[*argt].clone();

            let Value::TablePtr(argt) = argt else { panic!("function called with non-table argtable!") };
            let retptr = match f {
                Value::LuaFn(f_id, upvalues) => exec_fn(f_id, argt, &upvalues[..], ctxt),
                Value::NativeFn(i) => (NATIVE_FNS[i])(argt, ctxt),
                v => panic!("trying to execute non-function value! {:?}", v),
            };

            Value::TablePtr(retptr)
        },
        Expr::NewTable => Value::TablePtr(alloc_table(ctxt)),
        Expr::LitFunction(fnid) => {
            // evaluate all things we need to closure!
            let func = &ctxt.ir.fns[*fnid];
            let mut upvalues: Vec<Value> = Vec::new();
            for uref in &func.upvalue_refs {
                match uref {
                    UpvalueRef::Local(lid) => {
                        upvalues.push(ctxt.locals[*lid].clone());
                    },
                    UpvalueRef::Upvalue(uid) => {
                        upvalues.push(ctxt.upvalues[*uid].clone());
                    },
                }
            }

            Value::LuaFn(*fnid, upvalues)
        },
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

fn exec_body(statements: &[Statement], ctxt: &mut Ctxt) -> ControlFlow {
    for st in statements {
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
                }
            },
            ReturnTable(t) => {
                let t = ctxt.nodes[*t].clone();
                let Value::TablePtr(t) = t else { panic!("returning non-table!") };
                return ControlFlow::ReturnTable(t);
            },
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
                        ret@ControlFlow::ReturnTable(_) => return ret,
                    }
                }
            },
            Break => return ControlFlow::Break,
        }
    }

    ControlFlow::End
}

fn exec_fn(f: FnId, argtable: TablePtr, upvalues: &[Value], ctxt: &mut Ctxt) -> TablePtr {
    let mut locals = Vec::new();
    let mut nodes = Vec::new();
    let mut argtable = argtable;
    let mut upvalues = upvalues.to_vec();

    std::mem::swap(&mut locals, &mut ctxt.locals);
    std::mem::swap(&mut nodes, &mut ctxt.nodes);
    std::mem::swap(&mut argtable, &mut ctxt.argtable);
    std::mem::swap(&mut upvalues, &mut ctxt.upvalues);

    let flow = exec_body(&ctxt.ir.fns[f].body, ctxt);

    std::mem::swap(&mut locals, &mut ctxt.locals);
    std::mem::swap(&mut nodes, &mut ctxt.nodes);
    std::mem::swap(&mut argtable, &mut ctxt.argtable);
    std::mem::swap(&mut upvalues, &mut ctxt.upvalues);

    match flow {
        ControlFlow::Break => panic!("cannot break, if you are not in a loop!"),
        ControlFlow::ReturnTable(t) => t,
        ControlFlow::End => empty(ctxt),
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
    // temporary print
    eprintln!("{}", ir);

    let mut ctxt = Ctxt {
        heap: Vec::new(),
        globals: vec![Value::Nil; ir.globals.len()],
        ir,
        argtable: 0,
        locals: Vec::new(),
        nodes: Vec::new(),
        upvalues: Vec::new(),
    };

    if let Some(i) = ir.globals.iter().position(|x| x == "print") {
        ctxt.globals[i] = Value::NativeFn(0);
    }

    if let Some(i) = ir.globals.iter().position(|x| x == "next") {
        ctxt.globals[i] = Value::NativeFn(1);
    }

    if let Some(i) = ir.globals.iter().position(|x| x == "pairs") {
        ctxt.globals[i] = Value::NativeFn(2);
    }

    if let Some(i) = ir.globals.iter().position(|x| x == "type") {
        ctxt.globals[i] = Value::NativeFn(3);
    }

    let argtable = empty(&mut ctxt);

    exec_fn(ir.main_fn, argtable, &[], &mut ctxt);
}
