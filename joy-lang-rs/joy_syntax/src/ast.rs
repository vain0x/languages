use crate::{parse::Pos, tokenize::Token};
use bumpalo::{boxed::Box as BumpBox, collections::Vec as BumpVec, Bump};

#[derive(Debug)]
pub struct ALit<'b> {
    pub(crate) token: Token,
    pub(crate) text: &'b str,
    pub(crate) pos: Pos,
}

#[derive(Debug)]
pub struct AName<'b> {
    pub(crate) text: &'b str,
    pub(crate) pos: Pos,
}

#[derive(Debug)]
pub struct ABinaryExpr<'b> {
    pub(crate) op: BinaryOp,
    pub(crate) l: BoxedExpr<'b>,
    pub(crate) r: BoxedExpr<'b>,
    pub(crate) pos: Pos,
}

#[derive(Debug)]
pub enum AExpr<'b> {
    Lit(ALit<'b>),
    Name(AName<'b>),
    Binary(ABinaryExpr<'b>),
}

impl<'b> AExpr<'b> {
    pub fn boxed_in(self, bump: &'b Bump) -> BoxedExpr<'b> {
        BumpBox::new_in(self, bump)
    }
}

type BoxedExpr<'b> = BumpBox<'b, AExpr<'b>>;

#[derive(Debug)]
pub enum BinaryOp {
    Mul,
    Div,
    Modulo,
    Add,
    Sub,
}

#[derive(Debug)]
pub struct AExprDecl<'b>(pub AExpr<'b>);

#[derive(Debug)]
pub struct ADecl<'b>(pub AExprDecl<'b>);

#[derive(Debug)]
pub struct ARoot<'b> {
    pub decls: BumpVec<'b, ADecl<'b>>,
}
