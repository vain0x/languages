pub(crate) mod cmd;
pub(crate) mod gen_mir;
pub(crate) mod regalloc;

pub(crate) use self::cmd::*;
use crate::semantics::*;
use crate::Id;
use std::collections::BTreeMap;
use std::rc::Rc;

pub(crate) struct RegTag;
pub(crate) type RegId = Id<RegTag>;

pub(crate) struct LabelTag;
pub(crate) type LabelId = Id<LabelTag>;

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
    pub body_label_id: LabelId,
    pub end_label_id: LabelId,
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
    pub msgs: Vec<DocMsg>,
}

pub(crate) const NO_REG_ID: RegId = RegId::new(0);
pub(crate) const BASE_PTR_REG_ID: RegId = RegId::new(1);
#[allow(unused)]
pub(crate) const STACK_PTR_REG_ID: RegId = RegId::new(2);
#[allow(unused)]
pub(crate) const RET_REG_ID: RegId = RegId::new(3);
pub(crate) const KNOWN_REG_NUM: usize = 4;
pub(crate) const REG_NUM: usize = 12;
