#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum BinOp {
    Eq,
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum BinOpLevel {
    Eq,
    Mul,
    Add,
}

impl BinOp {
    pub(crate) fn level(self) -> BinOpLevel {
        match self {
            BinOp::Eq => BinOpLevel::Eq,
            BinOp::Mul => BinOpLevel::Mul,
            BinOp::Div => BinOpLevel::Mul,
            BinOp::Add => BinOpLevel::Add,
            BinOp::Sub => BinOpLevel::Add,
        }
    }
}

impl BinOpLevel {
    pub(crate) fn next(self) -> Option<BinOpLevel> {
        match self {
            BinOpLevel::Eq => Some(BinOpLevel::Add),
            BinOpLevel::Add => Some(BinOpLevel::Mul),
            BinOpLevel::Mul => None,
        }
    }
}
