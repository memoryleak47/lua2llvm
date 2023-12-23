use super::*;

pub fn declare_extra_fn(fname: &str, ret: ll::Type, args: &[ll::Type], ctxt: &mut Ctxt) {
    let args = args.iter().cloned().collect();
    let ty = ll::Type::Function(Box::new(ret), args);
    let f = ctxt.b.alloc_fn(fname, ty);
    ctxt.extra_fns.insert(fname.to_string(), f);
}

pub fn call_extra_fn(fname: &str, args: &[ll::ValueId], ctxt: &mut Ctxt) -> ll::ValueId {
    let f = ctxt.extra_fns[fname];

    let ll::ValueId::Global(gid) = f else { panic!() };
    let ll::GlobalDef::Function(_, ty, _) = &ctxt.b.m.global_defs[&gid] else { panic!() };

    ctxt.b.push_fn_call(f, args, ty.clone())
}
