use super::*;

pub(super) fn assemble_subexprs(mut subexprs: Vec<SubExpr>) -> Result<Expr, ()> {
    if subexprs.is_empty() { return Err(()); }
    loop {
        if let [SubExpr::Expr(x)] = &subexprs[..] {
            return Ok(x.clone());
        }

        // valid(i) == true iff i and it's required neighbour subexprs can be assembled together.
        let valid = |i: usize| {
            (!subexprs[i].left() || matches!(subexprs.get(i-1), Some(SubExpr::Expr(_))))
            &&
            (!subexprs[i].right() || matches!(subexprs.get(i+1), Some(SubExpr::Expr(_))))
        };

        let maxprio = (0..subexprs.len())
                        .filter(|&i| valid(i))
                        .map(|i| subexprs[i].prio())
                        .max()
                        .ok_or(())?;
        let mut i = (0..subexprs.len())
                        .filter(|&i| valid(i))
                        .find(|&i| subexprs[i].prio() == maxprio)
                        .ok_or(())?;
        if subexprs[i].assoc() == Assoc::Right {
            i = (0..subexprs.len())
                .rev()
                .filter(|&i| valid(i))
                .find(|&i| subexprs[i].prio() == maxprio)
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
        (SubExpr::Plus, Some(SubExpr::Expr(l)), Some(SubExpr::Expr(r))) =>
            Ok(Expr::Plus(bc(l), bc(r))),
        (SubExpr::CallArgs(args), Some(SubExpr::Expr(l)), None) =>
            Ok(Expr::FunctionCall {
                func: bc(l),
                args: args.clone(),
            }),
        _ => Err(()),
    }
}
