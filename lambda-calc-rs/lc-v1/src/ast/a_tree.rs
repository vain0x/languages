use crate::{
    semantics::local_symbol::NLocalSymbol, semantics::symbol::NSymbol,
    syntax::syntax_token::SyntaxToken, utils::*,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub(crate) struct AFnTy<'a> {
    pub(crate) param_tys: &'a [ATy<'a>],
    pub(crate) result_ty_opt: Option<&'a ATy<'a>>,
}

#[derive(Debug)]
pub(crate) enum ATy<'a> {
    Name(SyntaxToken<'a>),
    Fn(AFnTy<'a>),
}

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
    pub(crate) id: usize,
    pub(crate) params: BumpaloVec<'a, (SyntaxToken<'a>, Option<ATy<'a>>)>,
    pub(crate) result_ty_opt: Option<BumpaloBox<'a, ATy<'a>>>,
    pub(crate) body_opt: Option<BumpaloBox<'a, AExpr<'a>>>,
}

#[derive(Debug)]
pub(crate) enum AExpr<'a> {
    True(SyntaxToken<'a>),
    False(SyntaxToken<'a>),
    Number(SyntaxToken<'a>),
    Var(SyntaxToken<'a>),
    Call(ACallExpr<'a>),
    Block(BumpaloVec<'a, AStmt<'a>>),
    If(AIfExpr<'a>),
    Fn(AFnExpr<'a>),
}

#[derive(Debug)]
pub(crate) struct ALetStmt<'a> {
    pub(crate) name_opt: Option<SyntaxToken<'a>>,
    pub(crate) init_opt: Option<AExpr<'a>>,
}

#[derive(Debug)]
pub(crate) enum AStmt<'a> {
    Expr(AExpr<'a>),
    Let(ALetStmt<'a>),
}

#[derive(Debug)]
pub(crate) struct ARoot<'a> {
    pub(crate) stmts: BumpaloVec<'a, AStmt<'a>>,
    pub(crate) eof: SyntaxToken<'a>,
}

#[derive(Debug)]
pub(crate) struct Ast<'a> {
    pub(crate) root: ARoot<'a>,
    pub(crate) name_res: HashMap<usize, NSymbol>,
    pub(crate) fn_escapes: HashMap<usize, HashSet<NLocalSymbol>>,
}
