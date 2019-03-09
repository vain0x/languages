pub(crate) mod gen_mir;
pub(crate) mod regalloc;

use crate::*;

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
    ToStr,
    StrCat,
    ReadInt,
    ReadStr,
    Print,
    PrintLn,
    PrintLnInt,
    Alloc,
    Write,
    Exit,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum CmdArg {
    None,
    Int(i64),
    Ptr(usize, usize),
    Reg(RegId),
    Label(LabelId),
}

pub(crate) type Ins = (Cmd, RegId, CmdArg);

#[derive(Clone, Debug)]
pub(crate) struct GenFunDef {
    pub label_id: LabelId,
    pub inss: Vec<Ins>,
}

#[derive(Clone, Debug)]
pub(crate) struct Mir {
    pub sema: Rc<Sema>,
    pub reg_count: usize,
    pub label_count: usize,
    pub text: Vec<u8>,
    pub funs: BTreeMap<FunId, GenFunDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

#[derive(Clone)]
pub struct CompilationResult {
    pub success: bool,
    pub program: String,
    pub stderr: String,
}

pub(crate) const NO_REG_ID: RegId = RegId(0);
pub(crate) const BASE_PTR_REG_ID: RegId = RegId(1);
#[allow(unused)]
pub(crate) const STACK_PTR_REG_ID: RegId = RegId(2);
#[allow(unused)]
pub(crate) const RET_REG_ID: RegId = RegId(3);
pub(crate) const KNOWN_REG_NUM: usize = 4;
pub(crate) const REG_NUM: usize = 12;
