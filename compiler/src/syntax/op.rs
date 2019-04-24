use super::*;

/// Level of binary operators.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum OpLevel {
    Set,
    Range,
    LogOr,
    LogAnd,
    Cmp,
    Add,
    Mul,
    Anno,
}

/// Operator.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Op {
    Set,
    SetAdd,
    SetSub,
    SetMul,
    SetDiv,
    SetMod,
    Range,
    LogOr,
    LogAnd,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    BitOr,
    BitXor,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitShiftL,
    BitShiftR,
    Anno,
    As,
}

const OP_PUNS: &[(Op, Pun)] = &[
    (Op::Set, Pun::Set),
    (Op::SetAdd, Pun::SetAdd),
    (Op::SetSub, Pun::SetSub),
    (Op::SetMul, Pun::SetMul),
    (Op::SetDiv, Pun::SetDiv),
    (Op::SetMod, Pun::SetMod),
    (Op::Range, Pun::Range),
    (Op::LogOr, Pun::LogOr),
    (Op::LogAnd, Pun::LogAnd),
    (Op::Eq, Pun::Eq),
    (Op::Ne, Pun::Ne),
    (Op::Lt, Pun::Lt),
    (Op::Le, Pun::Le),
    (Op::Gt, Pun::Gt),
    (Op::Ge, Pun::Ge),
    (Op::Add, Pun::Add),
    (Op::Sub, Pun::Sub),
    (Op::BitOr, Pun::BitOr),
    (Op::BitXor, Pun::BitXor),
    (Op::Mul, Pun::Mul),
    (Op::Div, Pun::Div),
    (Op::Mod, Pun::Mod),
    (Op::BitAnd, Pun::BitAnd),
    (Op::BitShiftL, Pun::BitShiftL),
    (Op::BitShiftR, Pun::BitShiftR),
    (Op::Anno, Pun::Anno),
    (Op::As, Pun::As),
];

impl OpLevel {
    pub(crate) fn next_level(self) -> Option<Self> {
        match self {
            OpLevel::Set => Some(OpLevel::Range),
            OpLevel::Range => Some(OpLevel::LogOr),
            OpLevel::LogOr => Some(OpLevel::LogAnd),
            OpLevel::LogAnd => Some(OpLevel::Cmp),
            OpLevel::Cmp => Some(OpLevel::Add),
            OpLevel::Add => Some(OpLevel::Mul),
            OpLevel::Mul | OpLevel::Anno => None,
        }
    }

    pub(crate) fn pun_to_op(self, pun: Pun) -> Option<Op> {
        for &(op, op_pun) in OP_PUNS {
            if op_pun == pun && op.op_level() == self {
                return Some(op);
            }
        }
        None
    }
}

impl Op {
    fn op_level(self) -> OpLevel {
        match self {
            Op::Set => OpLevel::Set,
            Op::SetAdd => OpLevel::Set,
            Op::SetSub => OpLevel::Set,
            Op::SetMul => OpLevel::Set,
            Op::SetDiv => OpLevel::Set,
            Op::SetMod => OpLevel::Set,
            Op::Range => OpLevel::Range,
            Op::LogOr => OpLevel::LogOr,
            Op::LogAnd => OpLevel::LogAnd,
            Op::Eq => OpLevel::Cmp,
            Op::Ne => OpLevel::Cmp,
            Op::Lt => OpLevel::Cmp,
            Op::Le => OpLevel::Cmp,
            Op::Gt => OpLevel::Cmp,
            Op::Ge => OpLevel::Cmp,
            Op::Add => OpLevel::Add,
            Op::Sub => OpLevel::Add,
            Op::BitOr => OpLevel::Add,
            Op::BitXor => OpLevel::Add,
            Op::Mul => OpLevel::Mul,
            Op::Div => OpLevel::Mul,
            Op::Mod => OpLevel::Mul,
            Op::BitAnd => OpLevel::Mul,
            Op::BitShiftL => OpLevel::Mul,
            Op::BitShiftR => OpLevel::Mul,
            Op::Anno => OpLevel::Anno,
            Op::As => OpLevel::Anno,
        }
    }

    fn pun(self) -> Option<Pun> {
        OP_PUNS
            .iter()
            .filter_map(|&(op, pun)| if op == self { Some(pun) } else { None })
            .next()
    }

    pub(crate) fn is_stmt(self) -> bool {
        match self {
            Op::Set | Op::SetAdd | Op::SetSub | Op::SetMul | Op::SetDiv | Op::SetMod => true,
            _ => false,
        }
    }

    pub(crate) fn display(self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.pun() {
            Some(pun) => write!(f, "{}", pun.text()),
            None => write!(f, "{:?}", self),
        }
    }
}
