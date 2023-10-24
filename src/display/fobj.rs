use crate::ir::{self, IR, FnId, Statement, Expr, BinOpKind, Node, BlockId, Command};
use crate::infer::Infer;

use std::fmt::{self, Formatter};
use std::collections::HashMap;

const INLINE_CONST_NODES: bool = true;

pub struct FnDisplayObj<'ir, 'inf> {
    ir: &'ir IR,
    inf: Option<&'inf Infer>,
    const_nodes: HashMap<Node, Expr>,
}

impl<'ir, 'inf> FnDisplayObj<'ir, 'inf> {
    pub fn new(ir: &'ir IR, inf: Option<&'inf Infer>) -> Self {
        let const_nodes = Default::default();
        Self { ir, inf, const_nodes }
    }

    pub fn display_fn(&mut self, fid: FnId, f: &mut Formatter<'_>) -> fmt::Result {
        if self.ir.main_fn == fid {
            write!(f, "main function f{}:\n", fid)?;
        } else {
            write!(f, "function f{}:\n", fid)?;
        }

        for bid in 0..self.ir.fns[fid].blocks.len() {
            self.display_block(fid, bid, f)?;
        }

        write!(f, "end\n\n")?;

        Ok(())
    }

    fn display_block(&mut self, fid: FnId, bid: BlockId, f: &mut Formatter<'_>) -> fmt::Result {
        if bid == self.ir.fns[fid].start_block {
            write!(f, "  start block b{bid}:\n")?;
        } else {
            write!(f, "  block b{bid}:\n")?;
        }

        let statements = &self.ir.fns[fid].blocks[bid];
        for st in statements {
            self.display_statement(st, f)?;
        }

        Ok(())
    }

    fn display_statement(&mut self, st: &Statement, f: &mut Formatter<'_>) -> fmt::Result {
        use Statement::*;

        if INLINE_CONST_NODES {
            if let Compute(n, e) = st {
                if self.is_const(e) {
                    self.const_nodes.insert(*n, (*e).clone());
                    return Ok(());
                }
            }
        }

        let indent = "    ";

        write!(f, "{}", &indent)?;

        match st {
            Compute(n, e) => {
                write!(f, "{} = ", self.node_string(*n))?;
                self.display_expr(e, f)?;
            },
            Store(t, i, n) => {
                write!(f, "{}[{}] <- {}", self.node_string(*t), self.node_string(*i), self.node_string(*n))?;
            }
            If(cond, then_bid, else_bid) => {
                let cond = self.node_string(*cond);
                let then = self.block_id_string(*then_bid);
                let else_ = self.block_id_string(*else_bid);
                write!(f, "if {cond} then {then} else {else_}", )?;
            },
            FnCall(n, t) => write!(f, "{}({})", self.node_string(*n), self.node_string(*t))?,
            Command(cmd) => self.display_command(cmd, f)?,
            Return => write!(f, "return")?,
        }

        write!(f, ";\n")
    }

    fn display_command(&self, cmd: &Command, f: &mut Formatter<'_>) -> fmt::Result {
        match cmd {
            ir::Command::Print(v) => write!(f, "print({})", self.node_string(*v)),
            ir::Command::Throw(s) => write!(f, "throw('{s}')"),
        }
    }

    fn node_string(&self, node: Node) -> String {
        match self.const_nodes.get(&node) {
            Some(e) => {
                let mut s = String::new();
                let mut fmt = core::fmt::Formatter::new(&mut s);

                self.display_expr(e, &mut fmt).unwrap();

                s
            }
            None => format!("n{node}"),
        }
    }

    fn display_intrinsic(&self, intrinsic: &ir::Intrinsic, f: &mut Formatter<'_>) -> fmt::Result {
        match intrinsic {
            ir::Intrinsic::Type(v) => write!(f, "type({})", self.node_string(*v))?,
            ir::Intrinsic::Next(v1, v2) => write!(f, "next({}, {})", self.node_string(*v1), self.node_string(*v2))?,
        }

        Ok(())
    }

    fn display_expr(&self, expr: &Expr, f: &mut Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match expr {
            Index(t, i) => write!(f, "{}[{}]", self.node_string(*t), self.node_string(*i))?,
            Arg => write!(f, "arg")?,
            NewTable => write!(f, "{{}}")?,
            LitFunction(fid) => write!(f, "f{}", fid)?,
            Intrinsic(intrinsic) => self.display_intrinsic(intrinsic, f)?,
            BinOp(kind, l, r) => write!(f, "{} {} {}", self.node_string(*l), self.binop_string(kind), self.node_string(*r))?,
            Len(r) => write!(f, "#{}", self.node_string(*r))?,
            Num(x) => write!(f, "{}", x)?,
            Bool(b) => write!(f, "{}", b)?,
            Nil => write!(f, "nil")?,
            Str(s) => write!(f, "\"{}\"", s)?,
        }

        Ok(())
    }

    fn binop_string(&self, kind: &BinOpKind) -> &'static str {
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

    fn block_id_string(&self, block_id: BlockId) -> String {
        format!("b{block_id}")
    }

    fn is_const(&self, expr: &Expr) -> bool {
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
}
