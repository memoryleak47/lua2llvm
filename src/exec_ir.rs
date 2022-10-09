use crate::ir::{FnId, IR, Statement};

type TablePtr = usize;

type NativeFn = fn(TablePtr, &mut Ctxt) -> TablePtr;
type NativeFnId = usize;

static NATIVE_FNS: [NativeFn; 1] = [print_fn];

fn print_fn(t: TablePtr, ctxt: &mut Ctxt) -> TablePtr {
    todo!()
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
    heap: Vec<TableData>,
}

#[derive(Default)]
struct TableData {
    entries: Vec<(Value, Value)>,
    length: usize,
}

pub fn exec(ir: &IR) {
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


