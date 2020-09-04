use crate::{syntax::syntax_token::SyntaxToken, utils::*};

pub(crate) struct ACallExpr<'a> {
    pub(crate) callee: BumpaloBox<'a, AExpr<'a>>,
    pub(crate) args: BumpaloVec<'a, AExpr<'a>>,
}

pub(crate) struct AFnExpr<'a> {
    pub(crate) params: BumpaloVec<'a, SyntaxToken<'a>>,
    pub(crate) body: BumpaloBox<'a, AExpr<'a>>,
}

pub(crate) enum AExpr<'a> {
    Number(SyntaxToken<'a>),
    Var(SyntaxToken<'a>),
    Call(ACallExpr<'a>),
    Fn(AFnExpr<'a>),
}

pub(crate) struct ALetDecl<'a> {
    pub(crate) name_opt: Option<SyntaxToken<'a>>,
}

pub(crate) enum ADecl<'a> {
    Let(ALetDecl<'a>),
}

pub(crate) struct ARoot<'a> {
    pub(crate) decls: ADecl<'a>,
}

pub(crate) struct Ast<'a> {
    pub(crate) root: ARoot<'a>,
}
