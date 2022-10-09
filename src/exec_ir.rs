use crate::ir::{FnId, IR, Statement};

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

    // return empty table
    let ptr = ctxt.heap.len();
    ctxt.heap.push(TableData::default());
    ptr
}

fn table_get(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Value {
    ctxt.heap[ptr].entries.iter()
        .find(|(x, _)| *x == idx)
        .map(|(_, v)| v.clone())
        .unwrap_or(Value::Nil)
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

#[derive(Default)]
struct Ctxt {
    locals: Vec<Value>,
    nodes: Vec<Value>,
    heap: Vec<TableData>,
    globals: Vec<Value>,
}

#[derive(Default)]
struct TableData {
    entries: Vec<(Value, Value)>,
    length: usize,
}

pub fn exec(ir: &IR) {
    let mut ctxt = Ctxt::default();

    if let Some(i) = ir.globals.iter().position(|x| x == "print") {
        ctxt.globals[i] = Value::NativeFn(0);
    }

    for st in &ir.fns[ir.main_fn].body {
        use Statement::*;
        match st {
            Local(lid) => todo!(),
            Compute(n, expr) => todo!(),
            Store(lval, n) => todo!(),
            ReturnTable(n) => todo!(),
            If(n, then, els) => todo!(),
            Loop(body) => todo!(),
            Break => todo!(),
        }
    }
}


