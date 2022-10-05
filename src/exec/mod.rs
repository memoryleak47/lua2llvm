use std::collections::HashMap;

use crate::ast::*;

type TableData = Vec<(Value, Value)>;

#[derive(Default)]
struct Ctxt {
    globals: HashMap<String, Value>,
    heap: HashMap<TablePtr, TableData>,
    native_fns: Vec<fn(&[Value])>,
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
        let print = |vals: &[Value]| {
            for arg in vals {
                match arg {
                    Value::Nil => println!("nil"),
                    Value::Bool(b) => println!("{}", b),
                    Value::Str(s) => println!("\"{}\"", s),
                    Value::TablePtr(ptr) => println!("table {}", ptr),
                    Value::NativeFn(_) => println!("<native-fn>"),
                    Value::LuaFn(..) => println!("<lua-fn>"),
                    Value::Num(x) => println!("{}", x),
                }
            }
        };
        ctxt.native_fns.push(print);
        ctxt.globals.insert("print".to_string(), Value::NativeFn(0));
    }

    exec_body(&ast.statements, &mut HashMap::new(), &mut ctxt);
}

// exec body of function / if / while
fn exec_body(body: &[Statement], active_args: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> ControlFlow {
    for st in body {
        match st {
            Statement::Assign(lvalue, expr) => exec_statement_assign(lvalue, expr, active_args, ctxt),
            _ => todo!()
        }
    }

    ControlFlow::End
}

fn exec_statement_assign(lvalue: &LValue, expr: &Expr, active_args: &mut HashMap<String, Value>, ctxt: &mut Ctxt) {
    enum LValueRef {
        Var(String),
        TableIdx(TablePtr, Value),
    }
    let vref = match lvalue {
        LValue::Var(x) => LValueRef::Var(x.clone()),
        LValue::Dot(expr, field) => {
            let Value::TablePtr(ptr) = exec_expr(expr, active_args, ctxt) else { panic!("executing a.b where a is not a table") };
            let idx = Value::Str(field.clone());
            LValueRef::TableIdx(ptr, idx)
        },
        LValue::Index(expr, idx_expr) => {
            let Value::TablePtr(ptr) = exec_expr(expr, active_args, ctxt) else { panic!("executing a.b where a is not a table") };
            let idx = exec_expr(idx_expr, active_args, ctxt);
            LValueRef::TableIdx(ptr, idx)
        }
    };

    let res = exec_expr(expr, active_args, ctxt);

    match vref {
        LValueRef::Var(x) => {
            if let Some(v) = active_args.get_mut(&x) {
                *v = res;
            } else {
                ctxt.globals.insert(x, res);
            }
        },
        LValueRef::TableIdx(ptr, idx) => {
            let data = &mut ctxt.heap.get_mut(&ptr).expect("dangling pointer!");
            data.retain(|(x, _)| *x != idx);
            data.push((idx, res));
        }
    }
}

fn construct_table(fields: &[Field], active_args: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> Value {
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

    let mut data = TableData::new();
    for f in fields {
        match f {
            Field::Expr(expr) => {
                let val = exec_expr(expr, active_args, ctxt);
                let idx = lowest_free_table_idx(&data);
                data.push((idx, val));
            },
            Field::NameToExpr(name, expr) => {
                let val = exec_expr(expr, active_args, ctxt);
                let idx = Value::Str(name.clone());
                data.retain(|(x, _)| *x != idx);
                data.push((idx, val));
            },
            Field::ExprToExpr(idx, val) => {
                let idx = exec_expr(idx, active_args, ctxt);
                let val = exec_expr(val, active_args, ctxt);
                data.retain(|(x, _)| *x != idx);
                data.push((idx, val));
            },
        }
    }

    let ptr = lowest_free_tableptr(ctxt);
    ctxt.heap.insert(ptr, data);
    Value::TablePtr(ptr)
}

fn exec_expr(expr: &Expr, active_args: &mut HashMap<String, Value>, ctxt: &mut Ctxt) -> Value {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Num(x) => Value::Num(*x),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Function(args, body) => Value::LuaFn(args.clone(), body.clone()),
            Literal::Table(fields) => construct_table(fields, active_args, ctxt),
            Literal::Nil => Value::Nil,
        },
        Expr::LValue(lvalue) => todo!(),
        Expr::BinOp(kind, l, r) => todo!(),
        Expr::UnOp(kind, r) => todo!(),
        Expr::FunctionCall(call) => todo!(),
    }
}
