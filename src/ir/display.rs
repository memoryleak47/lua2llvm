use crate::ir::{IR, FnId, Statement, LValue, Expr, BinOpKind, UnOpKind};
use std::fmt::{self, Display, Formatter};

// functions: f<id>
// locals: l<id>
// globals: g<id>
// nodes: n<id>
// upvalues: u<id>

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for id in 0..self.fns.len() {
            display_fn(id, self, f)?;
        }

        Ok(())
    }
}

fn display_fn(id: FnId, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "f{}:\n", id)?;

    for st in &ir.fns[id].body {
        display_statement(st, 2, ir, f)?;
    }

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
        Local(lid) => write!(f, "local l{}\n", lid),
        Compute(n, e) => {
            write!(f, "n{} = ", n)?;
            display_expr(e, f)?;

            write!(f, "\n")
        },
        Store(l, n) => {
            display_lvalue(l, f)?;

            write!(f, " <- n{}\n", n)
        }
        ReturnTable(n) => write!(f, "return n{}\n", n),
        If(cond, body, else_body) => {
            write!(f, "if n{}:\n", cond)?;
            display_body(body, tabs+2, ir, f)?;
            write!(f, "{}else:\n", &indent)?;

            display_body(else_body, tabs+2, ir, f)
        },
        Loop(body) => {
            write!(f, "loop:\n")?;

            display_body(body, tabs+2, ir, f)
        },
        Break => write!(f, "break\n"),
    }
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

fn display_expr(expr: &Expr, f: &mut Formatter<'_>) -> fmt::Result {
    use Expr::*;
    match expr {
        LValue(l) => display_lvalue(l, f),
        Argtable => write!(f, "argtable"),
        FnCall(n, t) => write!(f, "n{}(n{})", n, t),
        NewTable => write!(f, "{{}}"),
        LitFunction(fid) => write!(f, "f{}", fid),
        BinOp(kind, l, r) => write!(f, "n{} {} n{}", l, display_binop(kind), r),
        UnOp(kind, r) => write!(f, "{} n{}", display_unop(kind), r),
        Num(x) => write!(f, "{}", x),
        Upvalue(uid) => write!(f, "u{}", uid),
        Bool(b) => write!(f, "{}", b),
        Nil => write!(f, "nil"),
        Str(s) => write!(f, "\"{}\"", s),
    }
}
