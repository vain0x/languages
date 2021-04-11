use bumpalo::{collections::Vec as BumpVec, boxed::Box as BumpBox};

pub struct Name<'b> { text: &'b str, index: usize }

type BoxedExpr<'b> = BumpBox<'b, AExpr<'b>>;

pub enum AExpr<'b> {
    Number(&'b str),
    Binary(BoxedExpr<'b>, BinaryOp, BoxedExpr<'b>),
}

pub enum BinaryOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}
