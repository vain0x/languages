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
    /// :>
    As,
}

pub(crate) const OPS: &[(&str, Op, OpLevel)] = &[
    ("=", Op::Set, OpLevel::Set),
    ("+=", Op::SetAdd, OpLevel::Set),
    ("-=", Op::SetSub, OpLevel::Set),
    ("*=", Op::SetMul, OpLevel::Set),
    ("/=", Op::SetDiv, OpLevel::Set),
    ("%=", Op::SetMod, OpLevel::Set),
    ("..", Op::Range, OpLevel::Range),
    ("||", Op::LogOr, OpLevel::LogOr),
    ("&&", Op::LogAnd, OpLevel::LogAnd),
    ("==", Op::Eq, OpLevel::Cmp),
    ("!=", Op::Ne, OpLevel::Cmp),
    ("<", Op::Lt, OpLevel::Cmp),
    ("<=", Op::Le, OpLevel::Cmp),
    (">", Op::Gt, OpLevel::Cmp),
    (">=", Op::Ge, OpLevel::Cmp),
    ("+", Op::Add, OpLevel::Add),
    ("-", Op::Sub, OpLevel::Add),
    ("|", Op::BitOr, OpLevel::Add),
    ("^", Op::BitXor, OpLevel::Add),
    ("*", Op::Mul, OpLevel::Mul),
    ("/", Op::Div, OpLevel::Mul),
    ("%", Op::Mod, OpLevel::Mul),
    ("&", Op::BitAnd, OpLevel::Mul),
    ("<<", Op::BitShiftL, OpLevel::Mul),
    (">>", Op::BitShiftR, OpLevel::Mul),
    (":", Op::Anno, OpLevel::Anno),
    (":>", Op::As, OpLevel::Anno),
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
        for &(_, op, op_level) in OPS {
            if op == the_op && op_level == self {
                return true;
            }
        }
        false
    }
}
