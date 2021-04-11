use crate::{parse::Pos, tokenize::Token};
use bumpalo::{boxed::Box as BumpBox, collections::Vec as BumpVec};

#[derive(Debug)]
pub struct ALit<'b> {
    pub(crate) token: Token,
    pub(crate) text: &'b str,
    pub(crate) pos: Pos,
}

pub struct Name<'b> {
    pub(crate) text: &'b str,
    pub(crate) index: usize,
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

#[derive(Debug)]
pub struct ADecl<'b>(pub ALit<'b>);

#[derive(Debug)]
pub struct ARoot<'b> {
    pub decls: BumpVec<'b, ADecl<'b>>,
}
