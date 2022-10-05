use std::collections::HashMap;

use crate::ast::*;

type TableData = Vec<(Value, Value)>;

#[derive(Default)]
struct Ctxt {
    globals: HashMap<String, Value>,
    heap: HashMap<TablePtr, TableData>,
    native_fns: Vec<fn(Vec<Value>) -> Value>,
}

enum ControlFlow {
    Break, // a break statement occured
    Return(Value), // a return statement occured
    End, // all statements were executed
}

type TablePtr = usize;

#[derive(Clone, PartialEq)]
enum Value {
    Nil,
    Bool(bool),
    TablePtr(TablePtr),
    Str(String),
    NativeFn(usize), // usize indexes in Ctxt::native_fns
    LuaFn(/*args: */ Vec<String>, /*body: */ Vec<Statement>),
    Num(f64),
}

pub fn exec(ast: &Ast) {
    let mut ctxt = Ctxt::default();

    { // add print
        let print = |vals: Vec<Value>| {
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
            Value::Nil
        };
        ctxt.native_fns.push(print);
        ctxt.globals.insert("print".to_string(), Value::NativeFn(0));
    }

    exec_body(&ast.statements, &mut HashMap::new(), &mut ctxt);
}

fn table_get(ptr: TablePtr, idx: Value, ctxt: &mut Ctxt) -> Value {
    ctxt.heap[&ptr].iter()
        .find(|(x, _)| *x == idx)
        .map(|(_, v)| v.clone())
        .unwrap_or(Value::Nil)
}

fn table_set(ptr: TablePtr, idx: Value, val: Value, ctxt: &mut Ctxt) {
    let data: &mut TableData = ctxt.heap.get_mut(&ptr).expect("table_set got dangling pointer!");
    data.retain(|(x, _)| *x != idx);
    data.push((idx, val));
}

// exec body of function / if / while
fn exec_body(body: &[Statement], locals: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> ControlFlow {
    for st in body {
        match st {
            Statement::Assign(lvalue, expr) => exec_statement_assign(lvalue, expr, locals, ctxt),
            Statement::FunctionCall(call) => { exec_function_call(call, locals, ctxt); },
            Statement::Local(var, optexpr) => {
                let val = optexpr.as_ref().map(|x| exec_expr(x, locals, ctxt)).unwrap_or(Value::Nil);
                locals.insert(var.clone(), val);
            },
            Statement::Return(expr) => {
                let val = exec_expr(expr, locals, ctxt);
                return ControlFlow::Return(val);
            }
            Statement::While(cond, body) => todo!(),
            Statement::If(ifblocks, optelse) => todo!(),
            Statement::Break => return ControlFlow::Break,
        }
    }

    ControlFlow::End
}

fn exec_function_call(call: &FunctionCall, locals: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> Value {
    let (func, argvals) = match call {
        FunctionCall::Direct(func, args) => {
            let func = exec_expr(func, locals, ctxt);
            let argvals: Vec<_> = args.iter().map(|x| exec_expr(x, locals, ctxt)).collect();

            (func, argvals)
        },
        FunctionCall::Colon(expr, field, args) => {
            // eval table
            let ptrval = exec_expr(expr, locals, ctxt);
            let Value::TablePtr(ptr) = ptrval else { panic!("using a:b() even though a is no table!") };

            // eval func
            let func = table_get(ptr, Value::Str(field.clone()), ctxt);

            // eval args
            let mut argvals = vec![ptrval];
            argvals.extend(args.iter().map(|x| exec_expr(x, locals, ctxt)));

            (func, argvals)
        },
    };

    match func {
        Value::LuaFn(args, body) => {
            let mut locals: HashMap<String, Value> = HashMap::new();
            for (k, v) in args.into_iter().zip(argvals.into_iter()) {
                locals.insert(k, v);
            }
            match exec_body(&body, &mut locals, ctxt) {
                ControlFlow::Return(v) => v,
                ControlFlow::Break => panic!("cannot break out of a function"),
                ControlFlow::End => Value::Nil,
            }
        },
        Value::NativeFn(i) => {
            let f = ctxt.native_fns[i];
            f(argvals)
        },
        _ => panic!("trying to call non-function"),
    }
}

fn exec_statement_assign(lvalue: &LValue, expr: &Expr, locals: &mut HashMap<String, Value>, ctxt: &mut Ctxt) {
    enum LValueRef {
        Var(String),
        TableIdx(TablePtr, Value),
    }
    let vref = match lvalue {
        LValue::Var(x) => LValueRef::Var(x.clone()),
        LValue::Dot(expr, field) => {
            let Value::TablePtr(ptr) = exec_expr(expr, locals, ctxt) else { panic!("executing a.b where a is not a table") };
            let idx = Value::Str(field.clone());
            LValueRef::TableIdx(ptr, idx)
        },
        LValue::Index(expr, idx_expr) => {
            let Value::TablePtr(ptr) = exec_expr(expr, locals, ctxt) else { panic!("executing a.b where a is not a table") };
            let idx = exec_expr(idx_expr, locals, ctxt);
            LValueRef::TableIdx(ptr, idx)
        }
    };

    let res = exec_expr(expr, locals, ctxt);

    match vref {
        LValueRef::Var(x) => {
            if let Some(v) = locals.get_mut(&x) {
                *v = res;
            } else {
                ctxt.globals.insert(x, res);
            }
        },
        LValueRef::TableIdx(ptr, idx) => table_set(ptr, idx, res, ctxt),
    }
}

fn construct_table(fields: &[Field], locals: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> Value {
    fn lowest_free_table_idx(d: &TableData) -> Value {
        for i in 1.. {
            let v = Value::Num(i as f64);
            if d.iter().all(|(x, _)| *x != v) {
                return v;
            }
        }
        unreachable!()
    }

    fn lowest_free_tableptr(ctxt: &Ctxt) -> TablePtr {
        for i in 1.. {
            if !ctxt.heap.contains_key(&i) {
                return i;
            }
        }
        unreachable!()
    }

    let ptr = lowest_free_tableptr(ctxt);
    ctxt.heap.insert(ptr, TableData::new());

    for f in fields {
        match f {
            Field::Expr(expr) => {
                let val = exec_expr(expr, locals, ctxt);
                let idx = lowest_free_table_idx(&ctxt.heap[&ptr]);
                table_set(ptr, idx, val, ctxt);
            },
            Field::NameToExpr(name, expr) => {
                let val = exec_expr(expr, locals, ctxt);
                let idx = Value::Str(name.clone());
                table_set(ptr, idx, val, ctxt);
            },
            Field::ExprToExpr(idx, val) => {
                let idx = exec_expr(idx, locals, ctxt);
                let val = exec_expr(val, locals, ctxt);
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
        _ => panic!("type error!"),
        // TODO add and & or
    }
}

fn exec_unop(kind: UnOpKind, r: Value, ctxt: &Ctxt) -> Value {
    use UnOpKind::*;
    match (kind, r) {
        (Neg, Value::Num(x)) => Value::Num(-x),
        (Len, Value::TablePtr(ptr)) => Value::Num(ctxt.heap[&ptr].len() as f64),
        (Len, Value::Str(s)) => Value::Num(s.len() as f64),
        _ => panic!("type error!"),
    }
    // TODO add not
}

fn exec_expr(expr: &Expr, locals: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> Value {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Num(x) => Value::Num(*x),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Function(args, body) => Value::LuaFn(args.clone(), body.clone()),
            Literal::Table(fields) => construct_table(fields, locals, ctxt),
            Literal::Nil => Value::Nil,
        },
        Expr::LValue(lvalue) => match &**lvalue {
            LValue::Var(var) => {
                if let Some(x) = locals.get(var) { return x.clone(); }
                ctxt.globals.get(var).cloned().unwrap_or(Value::Nil)
            },
            LValue::Dot(expr, field) => {
                let Value::TablePtr(ptr) = exec_expr(expr, locals, ctxt) else { panic!("trying a.b on non-table a!") };
                table_get(ptr, Value::Str(field.clone()), ctxt)
            },
            LValue::Index(expr, idx) => {
                let Value::TablePtr(ptr) = exec_expr(expr, locals, ctxt) else { panic!("trying a[b] on non-table a!") };
                let idx = exec_expr(idx, locals, ctxt);
                table_get(ptr, idx, ctxt)
            },
        },
        Expr::BinOp(kind, l, r) => {
            let l = exec_expr(l, locals, ctxt);
            let r = exec_expr(r, locals, ctxt);

            exec_binop(*kind, l, r)
        },
        Expr::UnOp(kind, r) => {
            let r = exec_expr(r, locals, ctxt);

            exec_unop(*kind, r, ctxt)
        },
        Expr::FunctionCall(call) => exec_function_call(call, locals, ctxt),
    }
}
