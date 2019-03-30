//! Picomet-lang IR: Intermediate representation for code generation.

use crate::semantics::*;
use crate::syntax::*;
use crate::Id;
use std::collections::BTreeMap;

pub(crate) mod gen;
pub(crate) mod reduce;

pub(crate) type BlockId = Id<Block>;

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

#[derive(Clone, Debug)]
pub(crate) struct Block {
    semi: Vec<Pir>,
    cond: Option<Pir>,
    next: BlockId,
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
    pub(crate) fn ty(&self) -> Ty {
        self.ty.clone()
    }

    pub(crate) fn exp_id(&self) -> ExpId {
        self.exp_id
    }

    pub(crate) fn int(value: i64, exp_id: ExpId) -> Pir {
        Pir {
            kind: PirKind::int(value),
            args: vec![],
            ty: Ty::int(),
            exp_id,
        }
    }

    fn into_op(self, op: Op, other: Pir) -> Pir {
        let ty = self.ty();
        let exp_id = self.exp_id;
        Pir {
            kind: PirKind::Op(op),
            args: vec![self, other],
            ty,
            exp_id,
        }
    }

    pub(crate) fn add(self, other: Pir) -> Pir {
        self.into_op(Op::Add, other)
    }

    pub(crate) fn add_int(self, value: i64) -> Pir {
        let other = Pir::int(value, self.exp_id);
        self.add(other)
    }

    pub(crate) fn sub(self, other: Pir) -> Pir {
        self.into_op(Op::Sub, other)
    }

    pub(crate) fn mul_int(self, value: i64) -> Pir {
        let other = Pir::int(value, self.exp_id);
        self.into_op(Op::Mul, other)
    }

    pub(crate) fn bit_and(self, other: Pir) -> Pir {
        self.into_op(Op::BitAnd, other)
    }

    pub(crate) fn bit_or(self, other: Pir) -> Pir {
        self.into_op(Op::BitOr, other)
    }

    pub(crate) fn bit_shift_l_int(self, value: i64) -> Pir {
        let other = Pir::int(value, self.exp_id);
        self.into_op(Op::BitShiftL, other)
    }

    pub(crate) fn bit_shift_r_int(self, value: i64) -> Pir {
        let other = Pir::int(value, self.exp_id);
        self.into_op(Op::BitShiftR, other)
    }

    /// `slice_new(xl, xr)` -> `xl | (xr << 32)`
    pub(crate) fn slice_new(self, end: Pir) -> Pir {
        self.bit_or(end.bit_shift_l_int(32))
    }

    /// `slice_begin(xs)` -> `xs & 0xFFFFFFFF`
    pub(crate) fn slice_begin(self) -> Pir {
        let mask = Pir::int(0xFFFF_FFFF_i64, self.exp_id);
        self.bit_and(mask)
    }

    /// `slice_end(xs)` -> `(xs >> 32) & 0xFFFFFFFF`
    pub(crate) fn slice_end(self) -> Pir {
        let mask = Pir::int(0xFFFF_FFFF_i64, self.exp_id);
        self.bit_shift_r_int(32).bit_and(mask)
    }

    pub(crate) fn into_deref(self) -> Pir {
        let (ty, exp_id) = (self.ty.clone(), self.exp_id);
        Pir {
            kind: PirKind::Deref,
            args: vec![self],
            ty,
            exp_id,
        }
    }
}
