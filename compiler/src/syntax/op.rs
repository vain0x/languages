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

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Op {
    Set,
    /// `+=`
    SetAdd,
    SetSub,
    SetMul,
    SetDiv,
    SetMod,
    Range,
    // `||`
    LogOr,
    // `&&`
    LogAnd,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    Add,
    Sub,
    // `|`
    BitOr,
    /// `^`
    BitXor,
    Mul,
    Div,
    Mod,
    // `&`
    BitAnd,
    // `<<`
    BitShiftL,
    // `>>`
    BitShiftR,
    // :
    Anno,
    As,
}

pub(crate) const OPS: &[(&str, Op)] = &[
    ("=", Op::Set),
    ("+=", Op::SetAdd),
    ("-=", Op::SetSub),
    ("*=", Op::SetMul),
    ("/=", Op::SetDiv),
    ("%=", Op::SetMod),
    ("..", Op::Range),
    ("||", Op::LogOr),
    ("&&", Op::LogAnd),
    ("==", Op::Eq),
    ("!=", Op::Ne),
    ("<", Op::Lt),
    ("<=", Op::Le),
    (">", Op::Gt),
    (">=", Op::Ge),
    ("+", Op::Add),
    ("-", Op::Sub),
    ("|", Op::BitOr),
    ("^", Op::BitXor),
    ("*", Op::Mul),
    ("/", Op::Div),
    ("%", Op::Mod),
    ("&", Op::BitAnd),
    ("<<", Op::BitShiftL),
    (">>", Op::BitShiftR),
    (":", Op::Anno),
    // Handled specially in tokenization.
    ("as", Op::As),
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

    pub(crate) fn contains(self, the_op: Op) -> bool {
        for &(_, op) in OPS {
            if op == the_op && op.op_level() == self {
                return true;
            }
        }
        false
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
}
