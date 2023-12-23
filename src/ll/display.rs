use std::fmt::{self, Display};
use crate::ll::*;

impl Display for StructId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}

impl Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0)
    }
}

impl Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl Display for GlobalValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for LocalValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueId::Global(g) => g.fmt(f),
            ValueId::Local(l) => l.fmt(f),
        }
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // display structs
        let mut sids: Vec<_> = self.structs.keys().cloned().collect();
        sids.sort_by_key(|x| x.0);

        for &sid in &sids {
            write!(f, "{} {{\n", sid)?;
            for x in &self.structs[&sid] {
                write!(f, "  {}\n", x)?;
            } 
            write!(f, "}}\n\n")?;
        }

        // display global-defs.
        let mut gids: Vec<_> = self.global_defs.keys().cloned().collect();
        gids.sort_by_key(|x| x.0);

        for &gid in &gids {
            write!(f, "{} = {}\n", gid, self.global_defs[&gid])?;
        }

        Ok(())
    }
}

impl Display for GlobalDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GlobalDef::String(s) => write!(f, "\"{}\"", s),
            GlobalDef::Function(name, ty, opt_impl) => {
                write!(f, "{}: {}", name, ty)?;
                if let Some(i) = opt_impl {
                    write!(f, " {{\n{}\n}}", i)?;
                }
                Ok(())
            },
        }
    }
}


impl Display for FnImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut vids: Vec<_> = self.vars.keys().cloned().collect();
        vids.sort_by_key(|x| x.0);

        for &vid in &vids {
            write!(f, "  let {}: {};\n", vid, self.vars[&vid])?;
        }

        let mut bids: Vec<_> = self.blocks.keys().cloned().collect();
        bids.sort_by_key(|x| x.0);

        for &bid in &bids {
            let opt_start = if bid == self.start_block { "start " } else { "" };
            write!(f, "  {}{}:\n", opt_start, bid)?;
            for st in &self.blocks[&bid] {
                write!(f, "    {};\n", st)?;
            }
        }

        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Compute(l, e) => write!(f, "{} = {}", l, e),
            Statement::PtrStore(val, ptr) => write!(f, "*{} = {}", ptr, val),
            Statement::Return(Some(x)) => write!(f, "return {}", x),
            Statement::Return(None) => write!(f, "return"),
            Statement::Unreachable => write!(f, "unreachable"),
            Statement::CondBr(cond, then, else_) => write!(f, "if {} then {} else {}", cond, then, else_),
            Statement::Br(target) => write!(f, "goto {}", target),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;

        match self {
            FnCall(fun, args, ty) => {
                write!(f, "call {} {}(", ty, fun)?;
                for i in 0..args.len() {
                    write!(f, "{}", &args[i])?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }

            NumOp(op_kind, num_kind, l, r) => {
                use NumOpKind::*;
                use NumKind::*;
                let op_kind = match op_kind {
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
                    IsNotEqual => "!=",
                };

                let num_kind = match num_kind {
                    Float => "f",
                    Int => "i",
                };

                write!(f, "{} {}{} {}", l, num_kind, op_kind, r)
            }

            PtrLoad(ty, vid) => write!(f, "*({} as {})", vid, ty),
            Not(v) => write!(f, "not {}", v),
            Or(l, r) => write!(f, "{} or {}", l, r),

            Var(vid) => write!(f, "{}", vid),
            Arg(i) => write!(f, "arg({})", i),

            PtrToInt(v, ty) => write!(f, "PtrToInt({}, {})", v, ty),
            IntToPtr(v, ty) => write!(f, "IntToPtr({}, {})", v, ty),
            BitCast(v, ty) => write!(f, "BitCast({}, {})", v, ty),
            ZExt(v, ty) => write!(f, "ZExt({}, {})", v, ty),

            ExtractValue(s, i) => write!(f, "ExtractValue({}, {})", s, i),
            InsertValue(s, v, i) => write!(f, "InsertValue({}, {}, {})", s, v, i),
            Poison(ty) => write!(f, "poison({})", ty),

            ConstReal(ty, val) => write!(f, "{}({})", ty, val),
            ConstInt(ty, val) => write!(f, "{}({})", ty, val),

            Gep(struct_ty, ptr, i) => write!(f, "Gep({}, {}, {})", struct_ty, ptr, i),
            SizeOf(ty) => write!(f, "SizeOf({})", ty),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Pointer(x) => write!(f, "{}*", x),
            Type::Struct(sid) => write!(f, "{}", sid),
            Type::Function(ret, args) => {
                write!(f, "fn(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() -1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ") -> {}", ret)
            },
            Type::Void => write!(f, "void"),
            Type::F64 => write!(f, "f64"),
            Type::I8 => write!(f, "i8"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
        }
    }
}
