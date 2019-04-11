//! Picomet-lang IR: Intermediate representation for code generation.

use crate::semantics::*;
use crate::syntax::*;
use crate::Id;
use std::collections::BTreeMap;
use std::rc::Rc;

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
    Op { op: Op, size: usize },
    Deref { size: usize },
    Return,
    If,
    Loop { loop_id: LoopId },
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
pub(crate) enum PirVarKind {
    Global { index: usize },
    Local { index: usize },
    Arg { index: usize },
}

#[derive(Clone, Debug)]
pub(crate) struct PirVarDef {
    is_temporary: bool,
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
    pub(crate) fn kind(&self) -> &PirKind {
        &self.kind
    }

    pub(crate) fn children(&self) -> &[Pir] {
        &self.args
    }

    pub(crate) fn ty(&self) -> Ty {
        self.ty.clone()
    }

    pub(crate) fn with_ty(self, ty: Ty) -> Pir {
        Pir { ty, ..self }
    }

    pub(crate) fn int(value: i64, exp_id: ExpId) -> Pir {
        Pir {
            kind: PirKind::int(value),
            args: vec![],
            ty: Ty::int(),
            exp_id,
        }
    }

    pub(crate) fn int_false(exp_id: ExpId) -> Pir {
        Pir::int(0, exp_id)
    }

    pub(crate) fn int_true(exp_id: ExpId) -> Pir {
        Pir::int(1, exp_id)
    }

    pub(crate) fn break_stmt(loop_id: LoopId, ty: Ty, exp_id: ExpId) -> Pir {
        Pir {
            kind: PirKind::Break { loop_id },
            args: vec![],
            ty,
            exp_id,
        }
    }

    fn into_op(self, op: Op, other: Pir) -> Pir {
        let ty = self.ty();
        let exp_id = self.exp_id;
        Pir {
            kind: PirKind::Op { op, size: 0 },
            args: vec![self, other],
            ty,
            exp_id,
        }
    }

    pub(crate) fn add(self, other: Pir) -> Pir {
        self.into_op(Op::Add, other)
    }

    pub(crate) fn sub(self, other: Pir) -> Pir {
        self.into_op(Op::Sub, other)
    }

    pub(crate) fn mul_int(self, value: i64) -> Pir {
        let other = Pir::int(value, self.exp_id);
        self.into_op(Op::Mul, other)
    }

    pub(crate) fn div_int(self, value: i64) -> Pir {
        let other = Pir::int(value, self.exp_id);
        self.into_op(Op::Div, other)
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
        let ptr_ty = self.ty(); // Missing pointer types, use slice type instead
        self.bit_or(end.bit_shift_l_int(32)).with_ty(ptr_ty)
    }

    /// `slice_begin(xs)` -> `xs & 0xFFFFFFFF`
    pub(crate) fn slice_begin(self) -> Pir {
        let ptr_ty = self.ty();
        let mask = Pir::int(0xFFFF_FFFF_i64, self.exp_id);
        self.bit_and(mask).with_ty(ptr_ty)
    }

    /// `slice_end(xs)` -> `(xs >> 32) & 0xFFFFFFFF`
    pub(crate) fn slice_end(self) -> Pir {
        let ptr_ty = self.ty();
        let mask = Pir::int(0xFFFF_FFFF_i64, self.exp_id);
        self.bit_shift_r_int(32).bit_and(mask).with_ty(ptr_ty)
    }

    pub(crate) fn into_deref(self) -> Pir {
        let (ty, exp_id) = (self.ty(), self.exp_id);
        Pir {
            kind: PirKind::Deref { size: 0 },
            args: vec![self],
            ty,
            exp_id,
        }
    }

    pub(crate) fn collect_loops(&self) -> Vec<LoopId> {
        fn dfs(pir: &Pir, loops: &mut Vec<LoopId>) {
            match pir.kind() {
                &PirKind::While { loop_id, .. }
                | &PirKind::Break { loop_id, .. }
                | &PirKind::Continue { loop_id, .. } => {
                    loops.push(loop_id);
                }
                _ => {}
            }

            for child in pir.children() {
                dfs(child, loops);
            }
        }

        let mut loops = vec![];
        dfs(self, &mut loops);
        loops.sort();
        loops.dedup();
        loops
    }
}

impl PirFunDef {
    pub(crate) fn args(&self) -> &[VarId] {
        &self.args
    }

    pub(crate) fn locals(&self) -> &[VarId] {
        &self.locals
    }

    pub(crate) fn body(&self) -> &Pir {
        &self.body
    }
}

impl PirProgram {
    pub(crate) fn var_kind(&self, var_id: VarId) -> PirVarKind {
        for (_, fun_def) in self.funs() {
            for (index, &arg_var_id) in fun_def.args().iter().enumerate() {
                if arg_var_id != var_id {
                    continue;
                }
                return PirVarKind::Arg { index };
            }
            for (index, &local_var_id) in fun_def.locals().iter().enumerate() {
                if local_var_id != var_id {
                    continue;
                }
                return if fun_def.is_global {
                    PirVarKind::Global { index }
                } else {
                    PirVarKind::Local { index }
                };
            }
        }
        panic!("Missing var {}", var_id)
    }

    pub(crate) fn funs(&self) -> impl Iterator<Item = (FunId, &PirFunDef)> {
        self.funs.iter().map(|(&fun_id, fun_def)| (fun_id, fun_def))
    }

    pub(crate) fn fun_body(&self, fun_id: FunId) -> &Pir {
        self.funs[&fun_id].body()
    }

    pub(crate) fn fun_local_count(&self, fun_id: FunId) -> usize {
        self.funs[&fun_id].locals.len()
    }
}

pub(crate) fn from_sema(sema: Rc<Sema>) -> PirProgram {
    let pir_program = self::gen::gen(sema);
    self::reduce::reduce(pir_program)
}
