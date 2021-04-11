use bumpalo::{boxed::Box as BumpBox, collections::Vec as BumpVec};

pub struct Name<'b> {
    text: &'b str,
    index: usize,
}

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

pub struct ADecl;

pub struct ARoot;
