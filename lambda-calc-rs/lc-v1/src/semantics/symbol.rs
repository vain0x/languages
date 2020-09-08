use super::{prim::Prim, prim_ty::PrimTy};

#[derive(Copy, Clone, Debug)]
pub(crate) enum NSymbol {
    Missing,
    Prim(Prim),
    PrimTy(PrimTy),
    StaticVar {
        id: usize,
        depth: usize,
        index: usize,
    },
    Param {
        id: usize,
        depth: usize,
        index: usize,
    },
    LocalVar {
        id: usize,
        depth: usize,
        index: usize,
    },
}
