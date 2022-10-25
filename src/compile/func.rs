use super::*;

pub fn compile_fn(val_f: LLVMValueRef, fn_id: FnId, ir: &IR, ctxt: &mut Ctxt) {
    unsafe {
        ctxt.bb = LLVMAppendBasicBlockInContext(ctxt.llctxt, val_f, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctxt.builder, ctxt.bb);

        let lit_f = &ir.fns[fn_id];
        for st in &lit_f.body {
            match st {
                Statement::Compute(n, e) => {
                    let vref = compile_expr(e, fn_id, ctxt);
                    ctxt.nodes.insert(*n, vref);
                },
                Statement::Store(t, i, v) => {
                    let t = alloc_val(ctxt.nodes[t], ctxt);
                    let i = alloc_val(ctxt.nodes[i], ctxt);
                    let v = alloc_val(ctxt.nodes[v], ctxt);
                    call_extra_fn("table_set", &[t, i, v], ctxt);
                },
                Statement::Return(v) => {
                    let v = ctxt.nodes[v];
                    let out = LLVMGetParam(val_f, 2);
                    LLVMBuildStore(ctxt.builder, v, out);
                    LLVMBuildRetVoid(ctxt.builder);
                },
                _ => {/* TODO */},
            }
        }
    }
}
