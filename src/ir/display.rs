use crate::ir::{IR, FnId, Statement, LValue, Expr, BinOpKind, UnOpKind, UpvalueRef};
use std::fmt::{self, Display, Formatter};

// functions: f<id>
// locals: l<id>
// globals: g<id>
// nodes: n<id>
// upvalues: u<id>

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for id in 0..self.fns.len() {
            display_fn(id, id == self.main_fn, self, f)?;
        }

        Ok(())
    }
}

fn display_fn(id: FnId, is_main: bool, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    if is_main {
        write!(f, "main function f{}:\n", id)?;
    } else {
        write!(f, "function f{}:\n", id)?;
    }

    for st in &ir.fns[id].body {
        display_statement(st, 2, ir, f)?;
    }

    write!(f, "end\n\n")?;

    Ok(())
}

fn display_body(statements: &[Statement], tabs: usize, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    for st in statements {
        display_statement(st, tabs, ir, f)?;
    }

    Ok(())
}

fn display_statement(st: &Statement, tabs: usize, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    use Statement::*;

    let mut indent = String::new();
    for _ in 0..tabs {
        indent.push(' ');
    }

    write!(f, "{}", &indent)?;

    match st {
        Local(lid) => write!(f, "local l{}", lid)?,
        Compute(n, e) => {
            write!(f, "n{} = ", n)?;
            display_expr(e, ir, f)?;
        },
        Store(l, n) => {
            display_lvalue(l, f)?;
            write!(f, " <- n{}", n)?;
        }
        Return(n) => write!(f, "return n{}", n)?,
        If(cond, body, else_body) => {
            write!(f, "if n{}:\n", cond)?;
            display_body(body, tabs+2, ir, f)?;
            write!(f, "{}else:\n", &indent)?;
            display_body(else_body, tabs+2, ir, f)?;
            write!(f, "{}end", &indent)?;
        },
        Loop(body) => {
            write!(f, "loop:\n")?;
            display_body(body, tabs+2, ir, f)?;
            write!(f, "{}end", &indent)?;
        },
        Break => write!(f, "break")?,
    }

    write!(f, ";\n")
}

fn display_lvalue(lvalue: &LValue, f: &mut Formatter<'_>) -> fmt::Result {
    use LValue::*;
    match lvalue {
        Local(lid) => write!(f, "l{}", lid),
        Global(gid) => write!(f, "g{}", gid),
        Index(t, i) => write!(f, "n{}[n{}]", t, i),
    }
}

fn display_binop(kind: &BinOpKind) -> &'static str {
    use BinOpKind::*;
    match kind {
        Plus => "+",
        Minus => "-",
        Mul => "*",
        Div => "/",
        Mod => "%",
        Lt => "<",
        Le => "<=",
        Gt => ">",
        Ge => ">=",
        IsEqual => "==",
        IsNotEqual => "~=",
        Concat => "..",
        Pow => "^",
    }
}

fn display_unop(kind: &UnOpKind) -> &'static str {
    use UnOpKind::*;
    match kind {
        Neg => "-",
        Len => "#",
        Not => "not ",
    }
}

fn display_expr(expr: &Expr, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    use Expr::*;
    match expr {
        LValue(l) => display_lvalue(l, f)?,
        Arg => write!(f, "arg")?,
        FnCall(n, t) => write!(f, "n{}(n{})", n, t)?,
        NewTable => write!(f, "{{}}")?,
        LitFunction(fid) => {
            write!(f, "f{}<", fid)?;
            let litfn = &ir.fns[*fid];
            for (i, x) in litfn.upvalue_refs.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                match x {
                    UpvalueRef::Upvalue(uid) => write!(f, "u{}", *uid)?,
                    UpvalueRef::Local(lid) => write!(f, "l{}", *lid)?,
                }
            }
            write!(f, ">")?;
        }
        NativeFn(s) => write!(f, "native fn \"{}\"", s)?,
        BinOp(kind, l, r) => write!(f, "n{} {} n{}", l, display_binop(kind), r)?,
        UnOp(kind, r) => write!(f, "{} n{}", display_unop(kind), r)?,
        Num(x) => write!(f, "{}", x)?,
        Upvalue(uid) => write!(f, "u{}", uid)?,
        Bool(b) => write!(f, "{}", b)?,
        Nil => write!(f, "nil")?,
        Str(s) => write!(f, "\"{}\"", s)?,
    }

    Ok(())
}
