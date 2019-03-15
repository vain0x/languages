#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum OpLevel {
    Set,
    Range,
    LogOr,
    Cmp,
    Add,
    Mul,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Op {
    Set,
    SetAdd,
    Range,
    // `||`
    LogOr,
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
    Mul,
    Div,
    Mod,
}

pub(crate) const OPS: &[(&str, Op, OpLevel)] = &[
    ("=", Op::Set, OpLevel::Set),
    ("+=", Op::SetAdd, OpLevel::Set),
    ("..", Op::Range, OpLevel::Range),
    ("||", Op::LogOr, OpLevel::LogOr),
    ("==", Op::Eq, OpLevel::Cmp),
    ("!=", Op::Ne, OpLevel::Cmp),
    ("<", Op::Lt, OpLevel::Cmp),
    ("<=", Op::Le, OpLevel::Cmp),
    (">", Op::Gt, OpLevel::Cmp),
    (">=", Op::Ge, OpLevel::Cmp),
    ("+", Op::Add, OpLevel::Add),
    ("-", Op::Sub, OpLevel::Add),
    ("|", Op::BitOr, OpLevel::Add),
    ("*", Op::Mul, OpLevel::Mul),
    ("/", Op::Div, OpLevel::Mul),
    ("%", Op::Mod, OpLevel::Mul),
];

impl OpLevel {
    pub(crate) fn next_level(self) -> Option<Self> {
        match self {
            OpLevel::Set => Some(OpLevel::Range),
            OpLevel::Range => Some(OpLevel::LogOr),
            OpLevel::LogOr => Some(OpLevel::Cmp),
            OpLevel::Cmp => Some(OpLevel::Add),
            OpLevel::Add => Some(OpLevel::Mul),
            OpLevel::Mul => None,
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
