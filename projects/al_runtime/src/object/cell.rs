#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum CellTy {
    Int,
    Bool,
    Cell,
}
