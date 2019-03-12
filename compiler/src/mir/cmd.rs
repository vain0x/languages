macro_rules! define_cmd {
    ($($name:ident,)*) => {
        #[derive(Clone, Copy, PartialEq, Debug)]
        pub(crate) enum Cmd {
            $($name),*
        }

        pub(crate) fn serialize_cmd(cmd: Cmd) -> &'static str {
            $(if cmd == Cmd::$name {
                return stringify!($name);
            })*
            unreachable!()
        }
    };
}

define_cmd! {
    Kill,
    Imm,
    AddImm,
    Mov,
    Store,
    Store8,
    Load,
    Load8,
    Push,
    Pop,
    PushRegs,
    PopRegs,
    Label,
    Jump,
    Unless,
    Call,
    Ret,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BitAnd,
    BitShiftR,
    ReadInt,
    ReadStr,
    PrintLnInt,
    Alloc,
    Write,
    Exit,
}
