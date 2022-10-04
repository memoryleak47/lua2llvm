use super::*;

pub(super) fn assemble_subexprs(mut subexprs: Vec<SubExpr>) -> Result<Expr, ()> {
    if subexprs.is_empty() { return Err(()); }
    loop {
        if let [SubExpr::Expr(x)] = &subexprs[..] {
            return Ok(x.clone());
        }

        let valids: Vec<usize> = (0..subexprs.len())
                .filter(|&i|
                    (!subexprs[i].left() || matches!(subexprs.get(i-1), Some(SubExpr::Expr(_))))
                    &&
                    (!subexprs[i].right() || matches!(subexprs.get(i+1), Some(SubExpr::Expr(_))))
                ).collect();

        let maxprio = valids.iter()
                        .map(|&i| subexprs[i].prio())
                        .max()
                        .ok_or(())?;
        let mut i: usize = *valids.iter()
                        .find(|&&i| subexprs[i].prio() == maxprio)
                        .ok_or(())?;
        if subexprs[i].assoc() == Assoc::Right {
            i = *valids.iter()
                .rfind(|&&i| subexprs[i].prio() == maxprio)
                .ok_or(())?;
        }
        // this is correct as subexprs with the same prio have the same assoc aswell.
        assert_eq!(subexprs[i].prio(), maxprio);

        let center = &subexprs[i];
        let (left, right) = (center.left(), center.right());
        let (mut l, mut r) = (None, None);
        if left { l = Some(&subexprs[i-1]); }
        if right { r = Some(&subexprs[i+1]); }
        let expr = assemble(center, l, r)?;
        let expr = SubExpr::Expr(expr);

        let mut range = i..(i+1);
        if left { range.start -= 1; }
        if right { range.end += 1; }

        subexprs.splice(range, std::iter::once(expr));
    }
}

fn assemble(center: &SubExpr, l: Option<&SubExpr>, r: Option<&SubExpr>) -> Result<Expr, ()> {
    let bc = |x: &Expr| -> Box<Expr> { Box::new(x.clone()) };
    match (center, l, r) {
        (SubExpr::BinOp(kind), Some(SubExpr::Expr(l)), Some(SubExpr::Expr(r))) =>
            Ok(Expr::BinOp(*kind, bc(l), bc(r))),
        (SubExpr::UnOp(kind), None, Some(SubExpr::Expr(r))) =>
            Ok(Expr::UnOp(*kind, bc(r))),
        (SubExpr::CallArgs(args), Some(SubExpr::Expr(l)), None) =>
            Ok(Expr::FunctionCall(bc(l), args.clone())),
        _ => Err(()),
    }
}
