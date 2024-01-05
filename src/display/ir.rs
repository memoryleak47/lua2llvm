use crate::ir::*;
use crate::display::ordered_map_iter;

use std::fmt::{self, Formatter, Display};

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (&fid, _) in ordered_map_iter(self.fns.iter()) {
            display_fn_header(fid, self, f)?;
            for (&bid, _) in ordered_map_iter(self.fns[&fid].blocks.iter()) {
                display_block_header(fid, bid, self, f)?;
                for st in &self.fns[&fid].blocks[&bid] {
                    write!(f, "{}", st)?;
                }
            }
            display_fn_footer(f)?;
        }

        for (loc, res) in &self.table_layouts {
            write!(f, "layout {} as {};\n", loc, res)?;
        }

        Ok(())
    }
}

pub fn display_fn_header(fid: FnId, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    let main_prefix = if ir.main_fn == fid { "main " } else { "" };

    write!(f, "{main_prefix}function f{fid}():\n")
}

pub fn display_fn_footer(f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "end\n\n")
}

pub fn display_block_header(fid: FnId, bid: BlockId, ir: &IR, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "  ")?;

    if bid == ir.fns[&fid].start_block {
        write!(f, "start block b{bid}:\n")?;
    } else {
        write!(f, "block b{bid}:\n")?;
    }

    Ok(())
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "    ")?;

        use Statement::*;

        match self {
            Compute(n, e) => {
                write!(f, "{} = {}", node_string(*n), e)?;
            },
            Store(t, i, n) => {
                write!(f, "{}[{}] <- {}", node_string(*t), node_string(*i), node_string(*n))?;
            }
            If(cond, then_bid, else_bid) => {
                let cond = node_string(*cond);
                let then = block_id_string(*then_bid);
                let else_ = block_id_string(*else_bid);
                write!(f, "if {cond} then {then} else {else_}", )?;
            },
            FnCall(n, t) => write!(f, "{}({})", node_string(*n), node_string(*t))?,
            Print(v) => write!(f, "print({})", node_string(*v))?,
            Throw(s) => write!(f, "throw('{s}')")?,
            Return => write!(f, "return")?,
        }

        write!(f, ";\n")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Index(t, i) => write!(f, "{}[{}]", node_string(*t), node_string(*i))?,
            Arg => write!(f, "arg")?,
            NewTable => write!(f, "{{}}")?,
            Function(fid) => write!(f, "{}", fn_id_string(*fid))?,
            Type(v) => write!(f, "type({})", node_string(*v))?,
            Next(v1, v2) => write!(f, "next({}, {})", node_string(*v1), node_string(*v2))?,
            BinOp(kind, l, r) => write!(f, "{} {} {}", node_string(*l), kind, node_string(*r))?,
            Len(r) => write!(f, "#{}", node_string(*r))?,
            Num(x) => write!(f, "{}", x)?,
            Bool(b) => write!(f, "{}", b)?,
            Nil => write!(f, "nil")?,
            Str(s) => write!(f, "\"{}\"", s)?,
        }

        Ok(())
    }
}

impl Display for BinOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BinOpKind::*;
        let s = match self {
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
        };
        write!(f, "{}", s)
    }
}

pub fn block_id_string(block_id: BlockId) -> String { format!("b{block_id}") }
pub fn fn_id_string(fid: FnId) -> String { format!("f{fid}") }
pub fn node_string(n: Node) -> String { format!("n{}", n) }


impl Display for TableLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TableLayout::HashTable => write!(f, "hashtable"),
            TableLayout::Struct(vals) => {
                write!(f, "struct {{")?;
                for (i, v) in vals.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")?;
                Ok(())
            }
        }
    }
}


