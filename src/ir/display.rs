use crate::ir::{self, IR, FnId, Statement, Expr, BinOpKind, Node, BlockId, Command};
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
    for (bid, blk) in ir.fns[id].blocks.iter().enumerate() {
        if bid == ir.fns[id].start_block {
            write!(f, "  start block b{bid}:\n")?;
        } else {
            write!(f, "  block b{bid}:\n")?;
        }
        display_block(blk, 4, &mut const_nodes, f)?;
    }

    write!(f, "end\n\n")?;

    Ok(())
}

fn display_block(statements: &[Statement], tabs: usize, const_nodes: &mut ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    for st in statements {
        display_statement(st, tabs, const_nodes, f)?;
    }

    Ok(())
}

fn display_statement(st: &Statement, tabs: usize, const_nodes: &mut ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
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
        If(cond, then_bid, else_bid) => {
            let cond = node_string(*cond, const_nodes);
            let then = block_id_string(*then_bid);
            let else_ = block_id_string(*else_bid);
            write!(f, "if {cond} then {then} else {else_}", )?;
        },
        FnCall(n, t) => write!(f, "{}({})", node_string(*n, const_nodes), node_string(*t, const_nodes))?,
        Command(cmd) => display_command(cmd, const_nodes, f)?,
        Return => write!(f, "return")?,
    }

    write!(f, ";\n")
}

fn display_command(cmd: &Command, const_nodes: &mut ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    match cmd {
        ir::Command::Print(v) => write!(f, "print({})", node_string(*v, const_nodes)),
        ir::Command::Throw(s) => write!(f, "throw('{s}')"),
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

fn display_intrinsic(intrinsic: &ir::Intrinsic, const_nodes: &ConstNodes, f: &mut Formatter<'_>) -> fmt::Result {
    match intrinsic {
        ir::Intrinsic::Type(v) => write!(f, "type({})", node_string(*v, const_nodes))?,
        ir::Intrinsic::Next(v1, v2) => write!(f, "next({}, {})", node_string(*v1, const_nodes), node_string(*v2, const_nodes))?,
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

fn block_id_string(block_id: BlockId) -> String {
    format!("b{block_id}")
}
