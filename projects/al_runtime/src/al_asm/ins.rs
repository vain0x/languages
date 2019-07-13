//! バーチャルマシンのアセンブリ

/// 実行指令の一種
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum InsKind {
    // 文
    Exit,
    Assert,
    CellSet,

    // 式
    Bool(bool),
    Int(i64),
    GlobalGet,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
}
