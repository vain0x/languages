use crate::syntax::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum AstKind {
    Null,
    True,
    Ident(String),
    Call,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Ast {
    kind: AstKind,
    children: Vec<Ast>,
    loc: SourceLocation,
}

impl Ast {
    pub(crate) fn new(kind: AstKind, children: Vec<Ast>, loc: SourceLocation) -> Self {
        Ast {
            kind,
            children,
            loc,
        }
    }

    pub(crate) fn kind(&self) -> &AstKind {
        &self.kind
    }

    pub(crate) fn children(&self) -> &[Ast] {
        &self.children
    }
}
