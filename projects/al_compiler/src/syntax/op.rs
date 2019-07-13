//! 演算子 (operator)

// 二項演算子 (binary operator)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum BinOp {
    Assign,
    Eq,
    Mul,
    Div,
    Add,
    Sub,
}

// 二項演算子の結合度を表す。結合が弱い方から強い方に並んでいる。
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum BinOpLevel {
    Assign,
    Eq,
    Mul,
    Add,
}

impl BinOp {
    pub(crate) fn level(self) -> BinOpLevel {
        match self {
            BinOp::Assign => BinOpLevel::Assign,
            BinOp::Eq => BinOpLevel::Eq,
            BinOp::Mul => BinOpLevel::Mul,
            BinOp::Div => BinOpLevel::Mul,
            BinOp::Add => BinOpLevel::Add,
            BinOp::Sub => BinOpLevel::Add,
        }
    }
}

impl BinOpLevel {
    pub(crate) fn first() -> BinOpLevel {
        BinOpLevel::Assign
    }

    pub(crate) fn next(self) -> Option<BinOpLevel> {
        match self {
            BinOpLevel::Assign => Some(BinOpLevel::Eq),
            BinOpLevel::Eq => Some(BinOpLevel::Add),
            BinOpLevel::Add => Some(BinOpLevel::Mul),
            BinOpLevel::Mul => None,
        }
    }
}
