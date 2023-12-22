use super::*;

pub fn declare_extra_fn(fname: &str, ret: ll::Type, args: &[ll::Type], ctxt: &mut Ctxt) {
    let args = args.iter().cloned().collect();
    let ty = ll::Type::Function(Box::new(ret), args);
    let f = ctxt.alloc_fn(fname.to_string(), ty);
    ctxt.extra_fns.insert(fname.to_string(), f);
}

pub fn call_extra_fn(fname: &str, args: &[ll::ValueId], ctxt: &mut Ctxt) -> ll::ValueId {
    let gid = ctxt.extra_fns[fname];
    let f = ll::ValueId::Global(gid);

    let args = args.iter().cloned().collect();

    let ll::GlobalDef::Function(_, ty, _) = &ctxt.m.global_defs[&gid] else { panic!() };

    ctxt.push_compute(ll::Expr::FnCall(f, args, ty.clone()))
}
