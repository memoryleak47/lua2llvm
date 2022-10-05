use std::collections::HashMap;

use crate::ast::*;

#[derive(Default)]
struct Ctxt {
    globals: HashMap<String, Value>,
    heap: HashMap<TablePtr, TableData>,
    native_fns: Vec<fn(Vec<Value>) -> Vec<Value>>,
    locals: Vec<HashMap<String, Value>>,
}

enum ControlFlow {
    Break, // a break statement occured
    Return(Vec<Value>), // a return statement occured
    End, // all statements were executed
}

type TablePtr = usize;

#[derive(Default)]
struct TableData {
    entries: Vec<(Value, Value)>,
    length: usize,
}

#[derive(Clone, PartialEq, Debug)]
enum Value {
    Nil,
    Bool(bool),
    TablePtr(TablePtr),
    Str(String),
    NativeFn(usize), // usize indexes in Ctxt::native_fns
    LuaFn(/*args: */ Vec<String>, /*body: */ Vec<Statement>),
    Num(f64),
}

fn truthy(v: &Value) -> bool {
    !matches!(v, Value::Bool(false) | Value::Nil)
}

pub fn exec(ast: &Ast) {
    let mut ctxt = Ctxt::default();
    ctxt.locals.push(Default::default());

    { // add print
        let print = |vals: Vec<Value>| -> Vec<Value> {
            for arg in vals {
                match arg {
                    Value::Nil => println!("nil"),
                    Value::Bool(b) => println!("{}", b),
                    Value::Str(s) => println!("{}", s),
                    Value::TablePtr(ptr) => println!("table {}", ptr),
                    Value::NativeFn(_) => println!("<native-fn>"),
                    Value::LuaFn(..) => println!("<lua-fn>"),
                    Value::Num(x) => println!("{}", x),
                }
            }
            Vec::new()
        };
        ctxt.native_fns.push(print);
        ctxt.globals.insert("print".to_string(), Value::NativeFn(0));
    }

    exec_body(&ast.statements, &mut ctxt);
}

fn table_get(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Value {
    ctxt.heap[&ptr].entries.iter()
        .find(|(x, _)| *x == idx)
        .map(|(_, v)| v.clone())
        .unwrap_or(Value::Nil)
}

fn table_set(ptr: TablePtr, idx: Value, val: Value, ctxt: &mut Ctxt) {
    if idx == Value::Nil {
        panic!("setting index with nil is forbidden in lua!");
    }

    let data: &mut TableData = ctxt.heap.get_mut(&ptr).expect("table_set got dangling pointer!");
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
        if idx == Value::Num((data.length+1) as f64) { // the next entry
            data.length += 1;
        }
        data.entries.push((idx, val));
        
    }
}

// exec body of function / if / while
fn exec_body(body: &[Statement], ctxt: &mut Ctxt) -> ControlFlow {
    for st in body {
        match st {
            Statement::Assign(lvalues, exprs) => exec_statement_assign(lvalues, exprs, ctxt),
            Statement::FunctionCall(call) => { exec_function_call(call, ctxt); },
            Statement::Local(vars, exprs) => {
                let mut vals = Vec::new();

                if let Some(expr) = exprs.last() {
                    for expr in exprs[..exprs.len()-1].iter() {
                        let val = exec_expr1(expr, ctxt);
                        vals.push(val);
                    }
                    let resvals = exec_expr(expr, ctxt);
                    vals.extend(resvals);
                }

                while vals.len() < vars.len() {
                    vals.push(Value::Nil);
                }
                for (var, val) in vars.iter().zip(vals.into_iter()) {
                    ctxt.locals.last_mut()
                        .unwrap()
                        .insert(var.clone(), val);
                }
            },
            Statement::Return(exprs) => {
                let mut vals = Vec::new();
                if let Some(expr) = exprs.last() {
                    for expr in exprs[..exprs.len()-1].iter() {
                        let val = exec_expr1(expr, ctxt);
                        vals.push(val);
                    }
                    let resvals = exec_expr(expr, ctxt);
                    vals.extend(resvals);
                }
                return ControlFlow::Return(vals);
            }
            Statement::Block(body) => {
                ctxt.locals.push(Default::default());
                let flow = exec_body(body, ctxt);
                ctxt.locals.pop();
                match flow {
                    ControlFlow::End => {},
                    ret => return ret,
                }
            },
            Statement::While(cond, body) => {
                let mut condval = exec_expr1(cond, ctxt);
                while truthy(&condval) {
                    ctxt.locals.push(Default::default());
                    let flow = exec_body(body, ctxt);
                    ctxt.locals.pop();
                    match flow {
                        ControlFlow::Break => break,
                        ret@ControlFlow::Return(_) => return ret,
                        ControlFlow::End => {},
                    }
                    condval = exec_expr1(cond, ctxt);
                }
            },
            Statement::Repeat(body, cond) => {
                loop {
                    ctxt.locals.push(Default::default());
                    let flow = exec_body(body, ctxt);
                    ctxt.locals.pop();
                    match flow {
                        ControlFlow::Break => break,
                        ret@ControlFlow::Return(_) => return ret,
                        ControlFlow::End => {},
                    }
                    let condval = exec_expr1(cond, ctxt);
                    if truthy(&condval) {
                        break;
                    }
                }
            }
            Statement::If(ifblocks, optelse) => {
                let mut done = false;
                for IfBlock(cond, body) in ifblocks {
                    let condval = exec_expr1(cond, ctxt);
                    if truthy(&condval) {
                        ctxt.locals.push(Default::default());
                        let flow = exec_body(body, ctxt);
                        ctxt.locals.pop();
                        match flow {
                            ControlFlow::End => {},
                            flow => return flow,
                        }

                        done = true;
                        break;
                    }
                }

                if let Some(body) = optelse {
                    if !done {
                        ctxt.locals.push(Default::default());
                        let flow = exec_body(body, ctxt);
                        ctxt.locals.pop();
                        match flow {
                            ControlFlow::End => {},
                            flow => return flow,
                        }
                    }
                }
            }
            Statement::Break => return ControlFlow::Break,
        }
    }

    ControlFlow::End
}

fn exec_function_call(call: &FunctionCall, ctxt: &mut Ctxt) -> Vec<Value> {
    let (func, argvals) = match call {
        FunctionCall::Direct(func, args) => {
            let func = exec_expr1(func, ctxt);
            let mut argvals = Vec::new();
            if let Some(arg) = args.last() {
                for arg in args[..args.len()-1].iter() {
                    let val = exec_expr1(arg, ctxt);
                    argvals.push(val);
                }
                let vals = exec_expr(arg, ctxt);
                argvals.extend(vals);
            }

            (func, argvals)
        },
        FunctionCall::Colon(expr, field, args) => {
            // eval table
            let ptrval = exec_expr1(expr, ctxt);
            let Value::TablePtr(ptr) = ptrval else { panic!("using a:b() even though a is no table!") };

            // eval func
            let func = table_get(ptr, Value::Str(field.clone()), ctxt);

            // eval args
            let mut argvals = vec![ptrval];
            if let Some(arg) = args.last() {
                for arg in args[..args.len()-1].iter() {
                    let val = exec_expr1(arg, ctxt);
                    argvals.push(val);
                }
                let vals = exec_expr(arg, ctxt);
                argvals.extend(vals);
            }

            (func, argvals)
        },
    };

    match func {
        Value::LuaFn(args, body) => {
            let mut map: HashMap<String, Value> = HashMap::new();
            for (k, v) in args.into_iter().zip(argvals.into_iter()) {
                map.insert(k, v);
            }
            let mut stack = vec![map];
            std::mem::swap(&mut stack, &mut ctxt.locals); // swap forward
            let flow = exec_body(&body, ctxt);
            ctxt.locals = stack; // swap back
            match flow {
                ControlFlow::Return(v) => v,
                ControlFlow::Break => panic!("cannot break out of a function"),
                ControlFlow::End => Vec::new(),
            }
        },
        Value::NativeFn(i) => {
            let f = ctxt.native_fns[i];
            f(argvals)
        },
        _ => panic!("trying to call non-function"),
    }
}

fn exec_statement_assign(lvalues: &[LValue], exprs: &[Expr], ctxt: &mut Ctxt) {
    enum LValueRef {
        Var(String),
        TableIdx(TablePtr, Value),
    }
    let mut valuerefs = Vec::new();
    for lvalue in lvalues {
        let vref = match lvalue {
            LValue::Var(x) => LValueRef::Var(x.clone()),
            LValue::Dot(expr, field) => {
                let Value::TablePtr(ptr) = exec_expr1(expr, ctxt) else { panic!("executing a.b where a is not a table") };
                let idx = Value::Str(field.clone());
                LValueRef::TableIdx(ptr, idx)
            },
            LValue::Index(expr, idx_expr) => {
                let Value::TablePtr(ptr) = exec_expr1(expr, ctxt) else { panic!("executing a.b where a is not a table") };
                let idx = exec_expr1(idx_expr, ctxt);
                LValueRef::TableIdx(ptr, idx)
            }
        };
        valuerefs.push(vref);
    }

    let mut values = Vec::new();
    for exp in exprs[..exprs.len()-1].iter() {
        let val = exec_expr1(exp, ctxt);
        values.push(val);
    }
    let lastvalues = exec_expr(exprs.last().unwrap(), ctxt);
    values.extend(lastvalues);

    while values.len() < valuerefs.len() {
        values.push(Value::Nil);
    }

    for (vref, v) in valuerefs.into_iter().zip(values.into_iter()) {
        match vref {
            LValueRef::Var(x) => {
                let mut done = false;
                for map in ctxt.locals.iter_mut().rev() {
                    if let Some(vptr) = map.get_mut(&x) {
                        *vptr = v.clone();
                        done = true;
                        break;
                    }
                }
                if !done {
                    ctxt.globals.insert(x, v);
                }
            },
            LValueRef::TableIdx(ptr, idx) => table_set(ptr, idx, v, ctxt),
        }
    }
}

fn construct_table(fields: &[Field], ctxt: &mut Ctxt) -> Value {
    fn lowest_free_tableptr(ctxt: &Ctxt) -> TablePtr {
        for i in 1.. {
            if !ctxt.heap.contains_key(&i) {
                return i;
            }
        }
        unreachable!()
    }

    let ptr = lowest_free_tableptr(ctxt);
    ctxt.heap.insert(ptr, TableData::default());

    let mut counter = 1; // the next Field::Expr id.
    for (i, f) in fields.iter().enumerate() {
        match f {
            Field::Expr(expr) => {
                let vals;
                if i == fields.len() - 1 {
                    vals = exec_expr(expr, ctxt);
                } else {
                    vals = vec![exec_expr1(expr, ctxt)];
                }

                for v in vals.into_iter() {
                    let idxval = Value::Num(counter as f64);
                    table_set(ptr, idxval, v.clone(), ctxt);

                    // this is here to account for a weird Lua quirk.
                    // namely that #{1, nil, 3} is actually 3, not 1.
                    if v != Value::Nil {
                        let data = ctxt.heap.get_mut(&ptr).unwrap();
                        if data.length < counter {
                            data.length = counter;
                        }
                    }
                    counter += 1;
                }
            },
            Field::NameToExpr(name, expr) => {
                let val = exec_expr1(expr, ctxt);
                let idx = Value::Str(name.clone());
                table_set(ptr, idx, val, ctxt);
            },
            Field::ExprToExpr(idx, val) => {
                let idx = exec_expr1(idx, ctxt);
                let val = exec_expr1(val, ctxt);
                table_set(ptr, idx, val, ctxt);
            },
        }
    }

    Value::TablePtr(ptr)
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

        (And, l, r) if truthy(&l) => r,
        (And, _, _r) => Value::Bool(false),
        (Or, l, _r) if truthy(&l) => Value::Bool(true),
        (Or, _, r) => r,
        _ => panic!("type error!"),
    }
}

fn exec_unop(kind: UnOpKind, r: Value, ctxt: &Ctxt) -> Value {
    use UnOpKind::*;
    match (kind, r) {
        (Neg, Value::Num(x)) => Value::Num(-x),
        (Len, Value::TablePtr(ptr)) => Value::Num(ctxt.heap[&ptr].length as f64),
        (Len, Value::Str(s)) => Value::Num(s.len() as f64),
        (Not, l) if truthy(&l) => Value::Bool(false),
        (Not, _) => Value::Bool(true),
        _ => panic!("type error!"),
    }
}

fn exec_expr(expr: &Expr, ctxt: &mut Ctxt) -> Vec<Value> {
    match expr {
        Expr::Literal(lit) => vec![match lit {
            Literal::Num(x) => Value::Num(*x),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Function(args, body) => Value::LuaFn(args.clone(), body.clone()),
            Literal::Table(fields) => construct_table(fields, ctxt),
            Literal::Nil => Value::Nil,
        }],
        Expr::LValue(lvalue) => match &**lvalue {
            LValue::Var(var) => {
                for map in ctxt.locals.iter().rev() {
                    if let Some(x) = map.get(var) { return vec![x.clone()]; }
                }
                vec![ctxt.globals.get(var).cloned().unwrap_or(Value::Nil)]
            },
            LValue::Dot(expr, field) => {
                let Value::TablePtr(ptr) = exec_expr1(expr, ctxt) else { panic!("trying a.b on non-table a!") };
                vec![table_get(ptr, Value::Str(field.clone()), ctxt)]
            },
            LValue::Index(expr, idx) => {
                let Value::TablePtr(ptr) = exec_expr1(expr, ctxt) else { panic!("trying a[b] on non-table a!") };
                let idx = exec_expr1(idx, ctxt);
                vec![table_get(ptr, idx, ctxt)]
            },
        },
        Expr::BinOp(kind, l, r) => {
            let l = exec_expr1(l, ctxt);
            let r = exec_expr1(r, ctxt);

            vec![exec_binop(*kind, l, r)]
        },
        Expr::UnOp(kind, r) => {
            let r = exec_expr1(r, ctxt);

            vec![exec_unop(*kind, r, ctxt)]
        },
        Expr::FunctionCall(call) => exec_function_call(call, ctxt),
        Expr::Ellipsis => todo!(),
    }
}

// exec_expr but shortened to one value
fn exec_expr1(expr: &Expr, ctxt: &mut Ctxt) -> Value {
    exec_expr(expr, ctxt)
        .first()
        .cloned()
        .unwrap_or(Value::Nil)
}
