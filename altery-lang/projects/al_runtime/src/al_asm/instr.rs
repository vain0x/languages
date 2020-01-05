//! バーチャルマシンのアセンブリ

/// 実行指令の一種
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum InstrKind {
    // 文
    Exit,
    Jump,
    JumpUnless,
    Call,
    Ret,
    Pop,
    Assert,
    CellSet,

    // 式
    Bool(bool),
    Int(i64),
    Label(usize),
    Pc(usize),
    GlobalGet,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
}
