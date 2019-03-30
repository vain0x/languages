//! Picomet-lang IR: Intermediate representation for code generation.

use crate::semantics::*;
use crate::syntax::*;
use std::collections::BTreeMap;

pub(crate) mod gen;

#[derive(Clone, Debug)]
pub(crate) enum PirVal {
    Unit,
    Byte(u8),
    Int(i64),
    Str(String),
}

#[derive(Clone, Debug)]
pub(crate) enum PirKind {
    Val(PirVal),
    Var { var_id: VarId },
    CallPrim(Prim),
    CallFun(FunId),
    IndexPoint,
    IndexSlice,
    Op(Op),
    Deref,
    Return,
    If,
    While { loop_id: LoopId },
    Break { loop_id: LoopId },
    Continue { loop_id: LoopId },
    Semi,
}

#[derive(Clone, Debug)]
pub(crate) struct Pir {
    kind: PirKind,
    args: Vec<Pir>,
    ty: Ty,
    exp_id: ExpId,
}

#[derive(Clone, Debug)]
pub(crate) struct PirVarDef {
    ty: Ty,
    exp_id: ExpId,
}

#[derive(Clone, Debug)]
pub(crate) struct PirFunDef {
    args: Vec<VarId>,
    locals: Vec<VarId>,
    body: Pir,
    is_global: bool,
    exp_id: ExpId,
}

#[derive(Clone, Debug)]
pub(crate) struct PirProgram {
    vars: BTreeMap<VarId, PirVarDef>,
    funs: BTreeMap<FunId, PirFunDef>,
}

impl PirKind {
    fn unit() -> PirKind {
        PirKind::Val(PirVal::Unit)
    }

    fn byte(value: u8) -> PirKind {
        PirKind::Val(PirVal::Byte(value))
    }

    fn int(value: i64) -> PirKind {
        PirKind::Val(PirVal::Int(value))
    }

    fn str(value: String) -> PirKind {
        PirKind::Val(PirVal::Str(value))
    }
}

impl Pir {
    fn into_deref(self) -> Pir {
        let (ty, exp_id) = (self.ty.clone(), self.exp_id);
        Pir {
            kind: PirKind::Deref,
            args: vec![self],
            ty,
            exp_id,
        }
    }
}
