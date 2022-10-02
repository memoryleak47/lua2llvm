use crate::ir::*;

#[derive(Clone)]
enum Value {
    Nil,
    NativeFn(fn(&[Value])),
    LuaFn(usize), // indexes into IR::fns
    Num(f64),
}

pub fn exec(ir: &IR) {
    let mut globals: Vec<Value> = vec![Value::Nil; ir.global_idents.len()];
    if let Some(i) = ir.global_idents.iter().position(|x| x == "print") {
        let print = |vals: &[Value]| {
            for arg in vals {
                match arg {
                    Value::Nil => println!("nil"),
                    Value::NativeFn(_) => println!("<native-fn>"),
                    Value::LuaFn(_) => println!("<lua-fn>"),
                    Value::Num(x) => println!("{}", x),
                }
            }
        };
        let print = Value::NativeFn(print);
        globals[i] = print;
    }

    exec_fn(ir, ir.main_idx, &mut [], &mut globals);
}

fn exec_fn(ir: &IR, fn_idx: usize, active_args: &mut [Value], globals: &mut [Value]) -> Value {
    let func = &ir.fns[fn_idx];
    for st in &func.body {
        match st {
            Statement::Assign(Var::Global(i), expr) => globals[*i] = exec_expr(expr, ir, active_args, globals),
            Statement::Assign(Var::FnArg(i), expr) => active_args[*i] = exec_expr(expr, ir, active_args, globals),
            Statement::FunctionCall(fnexpr, args) => {
                exec_fn_val(ir, fnexpr, args, active_args, globals);
            },
            Statement::Return(expr) => return exec_expr(expr, ir, active_args, globals),
        }
    }

    Value::Nil
}

fn exec_fn_val(ir: &IR, func: &Expr, args: &[Expr], active_args: &mut [Value], globals: &mut [Value]) -> Value {
    let func = exec_expr(func, ir, active_args, globals);
    let mut args: Vec<_> = args.iter().map(|x| exec_expr(x, ir, active_args, globals)).collect();
    match func {
        Value::NativeFn(f) => {
            f(&args);
            Value::Nil
        },
        Value::LuaFn(i) => exec_fn(ir, i, &mut args, globals),
        _ => panic!("trying to execute non-function"),
    }
}

fn exec_expr(expr: &Expr, ir: &IR, active_args: &mut [Value], globals: &mut [Value]) -> Value {
    match expr {
        Expr::Var(Var::Global(i)) => globals[*i].clone(),
        Expr::Var(Var::FnArg(i)) => active_args[*i].clone(),
        Expr::LiteralNum(x) => Value::Num(*x),
        Expr::Function(i) => Value::LuaFn(*i),
        Expr::Plus(l, r) => {
            let Value::Num(l) = exec_expr(l, ir, active_args, globals) else { panic!("non-numeric addition") };
            let Value::Num(r) = exec_expr(r, ir, active_args, globals) else { panic!("non-numeric addition") };
            Value::Num(l + r)
        },
        Expr::FunctionCall(func, args) => 
            exec_fn_val(ir, func, args, active_args, globals),
    }
}
