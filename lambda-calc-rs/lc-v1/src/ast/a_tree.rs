use crate::{syntax::syntax_token::SyntaxToken, utils::*};

#[derive(Debug)]
pub(crate) struct ACallExpr<'a> {
    pub(crate) callee: BumpaloBox<'a, AExpr<'a>>,
    pub(crate) args: BumpaloVec<'a, AExpr<'a>>,
}

#[derive(Debug)]
pub(crate) struct AIfExpr<'a> {
    pub(crate) cond_opt: Option<BumpaloBox<'a, AExpr<'a>>>,
    pub(crate) body_opt: Option<BumpaloBox<'a, AExpr<'a>>>,
    pub(crate) alt_opt: Option<BumpaloBox<'a, AExpr<'a>>>,
}

#[derive(Debug)]
pub(crate) struct AFnExpr<'a> {
    pub(crate) params: BumpaloVec<'a, SyntaxToken<'a>>,
    pub(crate) body_opt: Option<BumpaloBox<'a, AExpr<'a>>>,
}

#[derive(Debug)]
pub(crate) enum AExpr<'a> {
    True(SyntaxToken<'a>),
    False(SyntaxToken<'a>),
    Number(SyntaxToken<'a>),
    Var(SyntaxToken<'a>),
    Call(ACallExpr<'a>),
    If(AIfExpr<'a>),
    Fn(AFnExpr<'a>),
}

#[derive(Debug)]
pub(crate) struct ALetDecl<'a> {
    pub(crate) name_opt: Option<SyntaxToken<'a>>,
    pub(crate) init_opt: Option<AExpr<'a>>,
}

#[derive(Debug)]
pub(crate) enum ADecl<'a> {
    Expr(AExpr<'a>),
    Let(ALetDecl<'a>),
}

#[derive(Debug)]
pub(crate) struct ARoot<'a> {
    pub(crate) decls: BumpaloVec<'a, ADecl<'a>>,
    pub(crate) eof: SyntaxToken<'a>,
}

#[derive(Debug)]
pub(crate) struct Ast<'a> {
    pub(crate) root: ARoot<'a>,
}
