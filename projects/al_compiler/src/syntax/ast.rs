use crate::syntax::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum AstKind {
    True,
    False,
    Ident(String),
    Int(i64),
    Assert,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Call,
    Semi,
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

    pub(crate) fn loc(&self) -> SourceLocation {
        self.loc
    }

    pub(crate) fn total_loc(&self) -> SourceLocation {
        fn find_start(ast: &Ast) -> usize {
            ast.children().first().map(find_start).unwrap_or(ast.loc().start())
        }

        fn find_end(ast: &Ast) -> usize {
            ast.children().last().map(find_end).unwrap_or(ast.loc().end())
        }

        SourceLocation::new(self.loc().file(), find_start(self), find_end(self))
    }
}
