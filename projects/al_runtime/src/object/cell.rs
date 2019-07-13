#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CellTy {
    None,
    Bool,
    Int,
    Cell,
}
