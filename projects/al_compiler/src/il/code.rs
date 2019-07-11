pub(crate) enum Code {
    Exit,
    Assert,
    PushTrue,
    PushFalse,
    PushInt(i64),
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
}
