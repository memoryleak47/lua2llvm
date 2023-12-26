use super::*;

fn compile_block(fid: FnId, bid: BlockId, ctxt: &mut Ctxt) {
    let f = ctxt.ir.fns[&fid].clone();
    let blk = &f.blocks[&bid];

    ctxt.b.set_active_block(ctxt.blocks[&bid]);

    for (sid, st) in blk.iter().enumerate() {
        match st {
            Statement::Compute(n, e) => {
                let vref = compile_expr(e, (fid, bid, sid), ctxt);
                ctxt.nodes.insert(*n, vref);
            },
            Statement::Store(t, i, v) => {
                if let Some(field_ptr) = infer_struct_field_ptr(*t, *i, (fid, bid, sid), ctxt) {
                    let v = ctxt.nodes[v];
                    ctxt.b.push_ptr_store(v, field_ptr);
                } else {
                    let t = alloc_val(ctxt.nodes[t], ctxt);
                    let i = alloc_val(ctxt.nodes[i], ctxt);
                    let v = alloc_val(ctxt.nodes[v], ctxt);
                    call_extra_fn("table_set", &[t, i, v], ctxt);
                }
            },
            Statement::If(cond, then_bid, else_bid) => {
                let cond = ctxt.nodes[cond];
                let cond = extract_bool(cond, ctxt);

                let thenbb = ctxt.blocks[then_bid];
                let elsebb = ctxt.blocks[else_bid];
                ctxt.b.push_cond_br(cond, thenbb, elsebb);
            },
            Statement::FnCall(f, arg) => {
                let f = ctxt.nodes[f];
                let arg = ctxt.nodes[arg];

                fn_call(f, arg, ctxt);
            }
            Statement::Print(var) => {
                let t = alloc_val(ctxt.nodes[var], ctxt);
                call_extra_fn("print", &[t], ctxt);
            },
            Statement::Throw(s) => {
                let v = ctxt.b.alloc_string(s);
                call_extra_fn("throw_", &[v], ctxt);
                ctxt.b.push_return_void()
            },
            Statement::Return => {
                ctxt.b.push_return_void()
            },
        }
    }
}

pub fn infer_struct_field_ptr(t: Node, i: Node, stmt: Stmt, ctxt: &mut Ctxt) -> Option<ll::ValueId> {
    let t_val = merged_value(t, stmt, &ctxt.inf);
    let i_val = merged_value(i, stmt, &ctxt.inf);
    let loc = single_loc_value(&t_val)?;
    let TableLayout::Struct(values) = &ctxt.layout.table_layouts[&loc] else { return None };

    let i = values.iter().position(|value| value == &i_val).unwrap();

    let struct_ty = ctxt.layout_structs[&loc].clone();
    let t = ctxt.nodes[&t];
    let t = ctxt.b.push_extract_value(t, 1);
    let t = ctxt.b.push_int_to_ptr(t, ll::Type::Pointer(Box::new(struct_ty.clone())));
    let field_ptr = ctxt.b.push_gep(struct_ty, t, i);

    Some(field_ptr)
}

fn fn_call(f_val: ll::ValueId /* Value with FN tag */, arg: ll::ValueId, ctxt: &mut Ctxt) {
    // check tag
    let t = tag_err(f_val, Tag::FN, ctxt);
    err_chk(t, "trying to call non-function!", ctxt);

    // call fn
    let f /* i64 */ = ctxt.b.push_extract_value(f_val, 1);
    let f /* void (*fn)(Value) */ = ctxt.b.push_int_to_ptr(f, ctxt.v2void_ptr_t());
    let args = vec![alloc_val(arg, ctxt)];
    ctxt.b.push_fn_call(f, args, ctxt.v2void_t());
}

pub fn compile_fn(fid: FnId, ctxt: &mut Ctxt) {
    ctxt.blocks = Default::default();
    ctxt.nodes = Default::default();

    ctxt.b.set_active_fn(ctxt.lit_fns[&fid]);

    let f = ctxt.ir.fns[&fid].clone();

    let mut bids: Vec<BlockId> = f.blocks.keys().cloned().collect();
    bids.sort();

    for &bid in &bids {
        let new_blk = ctxt.b.alloc_block();
        ctxt.blocks.insert(bid, new_blk);
    }

    ctxt.b.set_start_block(ctxt.blocks[&f.start_block]);

    for &bid in &bids {
        compile_block(fid, bid, ctxt);
    }
}
