use crate::ir::{self, IR, FnId, Statement, Expr, BinOpKind, Node};
use std::fmt::{self, Display, Formatter};
use std::collections::HashMap;

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

fn is_const(expr: &Expr) -> bool {
    match expr {
        Expr::Index(_, _) => false,
        Expr::Arg => true,
        Expr::NewTable => false,
        Expr::LitFunction(_) => true,
        Expr::BinOp(_, _, _) => true,
        Expr::Len(_) => false,
        Expr::Intrinsic(_) => false,
        Expr::Num(_) => true,
        Expr::Bool(_) => true,
        Expr::Nil => true,
        Expr::Str(_) => true,
    }
}

type ConstNodes = HashMap<Node, Expr>;

fn display_fn(id: FnId, is_main: bool, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    if is_main {
        write!(f, "main function f{}:\n", id)?;
    } else {
        write!(f, "function f{}:\n", id)?;
    }

    let mut const_nodes = ConstNodes::new();
    for st in &ir.fns[id].body {
        display_statement(st, 2, ir, &mut const_nodes, f)?;
    }

    write!(f, "end\n\n")?;

    Ok(())
}

fn display_body(statements: &[Statement], tabs: usize, ir: &IR, const_nodes: &mut ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    for st in statements {
        display_statement(st, tabs, ir, const_nodes, f)?;
    }

    Ok(())
}

fn display_statement(st: &Statement, tabs: usize, ir: &IR, const_nodes: &mut ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    use Statement::*;

    if let Compute(n, e) = st {
        if is_const(e) {
            const_nodes.insert(*n, (*e).clone());
            return Ok(());
        }
    }

    let mut indent = String::new();
    for _ in 0..tabs {
        indent.push(' ');
    }

    write!(f, "{}", &indent)?;

    match st {
        Compute(n, e) => {
            write!(f, "{} = ", node_string(*n, const_nodes))?;
            display_expr(e, const_nodes, f)?;
        },
        Store(t, i, n) => {
            write!(f, "{}[{}] <- {}", node_string(*t, const_nodes), node_string(*i, const_nodes), node_string(*n, const_nodes))?;
        }
        Return => write!(f, "return")?,
        If(cond, body, else_body) => {
            write!(f, "if {}:\n", node_string(*cond, const_nodes))?;
            display_body(body, tabs+2, ir, const_nodes, f)?;
            write!(f, "{}else:\n", &indent)?;
            display_body(else_body, tabs+2, ir, const_nodes, f)?;
            write!(f, "{}end", &indent)?;
        },
        Loop(body) => {
            write!(f, "loop:\n")?;
            display_body(body, tabs+2, ir, const_nodes, f)?;
            write!(f, "{}end", &indent)?;
        },
        FnCall(n, t) => write!(f, "{}({})", node_string(*n, const_nodes), node_string(*t, const_nodes))?,
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

fn display_intrinsic(intrinsic: &ir::Intrinsic, const_nodes: &ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    match intrinsic {
        ir::Intrinsic::Print(v) => write!(f, "print({})", node_string(*v, const_nodes))?,
        ir::Intrinsic::Type(v) => write!(f, "type({})", node_string(*v, const_nodes))?,
        ir::Intrinsic::Next(v1, v2) => write!(f, "next({}, {})", node_string(*v1, const_nodes), node_string(*v2, const_nodes))?,
        ir::Intrinsic::Throw(s) => write!(f, "throw('{s}')")?,
    }

    Ok(())
}

fn display_expr(expr: &Expr, const_nodes: &ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    use Expr::*;
    match expr {
        Index(t, i) => write!(f, "{}[{}]", node_string(*t, const_nodes), node_string(*i, const_nodes))?,
        Arg => write!(f, "arg")?,
        NewTable => write!(f, "{{}}")?,
        LitFunction(fid) => write!(f, "f{}", fid)?,
        Intrinsic(intrinsic) => display_intrinsic(intrinsic, const_nodes, f)?,
        BinOp(kind, l, r) => write!(f, "{} {} {}", node_string(*l, const_nodes) , display_binop(kind), node_string(*r, const_nodes))?,
        Len(r) => write!(f, "#{}", node_string(*r, const_nodes))?,
        Num(x) => write!(f, "{}", x)?,
        Bool(b) => write!(f, "{}", b)?,
        Nil => write!(f, "nil")?,
        Str(s) => write!(f, "\"{}\"", s)?,
    }

    Ok(())
}

fn node_string(node: Node, const_nodes: &ConstNodes) -> String {
    match const_nodes.get(&node) {
        Some(e) => {
            let mut s = String::new();
            let mut fmt = core::fmt::Formatter::new(&mut s);

            display_expr(e, const_nodes, &mut fmt).unwrap();

            s
        }
        None => format!("n{node}"),
    }
}
