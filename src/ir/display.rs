use crate::ir::{self, IR, FnId, Statement, Expr, BinOpKind};
use std::fmt::{self, Display, Formatter};

// functions: f<id>
// nodes: n<id>

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
        Compute(n, e) => {
            write!(f, "n{} = ", n)?;
            display_expr(e, f)?;
        },
        Store(t, i, n) => {
            write!(f, "n{}[n{}] <- n{}", t, i, n)?;
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

fn display_intrinsic(intrinsic: &ir::Intrinsic, f: &mut Formatter<'_>) -> fmt::Result {
    match intrinsic {
        ir::Intrinsic::Print(v) => write!(f, "print({})", v)?,
        ir::Intrinsic::Type(v) => write!(f, "type({})", v)?,
        ir::Intrinsic::Next(v1, v2) => write!(f, "next({}, {})", v1, v2)?,
    }

    Ok(())
}

fn display_expr(expr: &Expr, f: &mut Formatter<'_>) -> fmt::Result {
    use Expr::*;
    match expr {
        Index(t, i) => write!(f, "n{}[n{}]", t, i)?,
        Arg => write!(f, "arg")?,
        FnCall(n, t) => write!(f, "n{}(n{})", n, t)?,
        NewTable => write!(f, "{{}}")?,
        LitFunction(fid) => write!(f, "f{}", fid)?,
        Intrinsic(intrinsic) => display_intrinsic(intrinsic, f)?,
        BinOp(kind, l, r) => write!(f, "n{} {} n{}", l, display_binop(kind), r)?,
        Len(r) => write!(f, "#n{}", r)?,
        Num(x) => write!(f, "{}", x)?,
        Bool(b) => write!(f, "{}", b)?,
        Nil => write!(f, "nil")?,
        Str(s) => write!(f, "\"{}\"", s)?,
    }

    Ok(())
}
