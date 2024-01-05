mod hir;
mod ir;

mod infer;
pub use infer::infer_to_string;

fn ordered_map_iter<'s, K: Ord + 's, V: 's>(it: impl Iterator<Item=(&'s K, &'s V)>,) -> impl Iterator<Item=(&'s K, &'s V)> {
    let mut v: Vec<_> = it.collect();
    v.sort_by_key(|x| x.0);
    v.into_iter()
}
