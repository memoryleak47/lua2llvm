use crate::ir::IR;
use crate::infer::Infer;

#[derive(PartialEq, Eq)]
enum Changed { Yes, No }
type Optimization = fn(&mut IR, &mut Infer) -> Changed;

static OPTIMIZATIONS: &'static [Optimization] = &[];

pub fn optimize(ir: &mut IR, inf: &mut Infer) {
    loop {
        let mut changed = Changed::No;
        for o in OPTIMIZATIONS {
            if o(ir, inf) == Changed::Yes {
                changed = Changed::Yes;
            }
        }

        if changed == Changed::No { break; }
    }
}
