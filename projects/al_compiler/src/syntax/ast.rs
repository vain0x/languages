use crate::syntax::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum AstKind {
    True,
    False,
    Ident(String),
    Int(i64),
    Assert,
    Bin(BinOp),
    Call,
    Semi,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Ast {
    kind: AstKind,
    children: Vec<Ast>,

    // ノードの中心的な意味を持つ範囲。例えば `2 + 3` の `+` の部分。
    main_loc: SourceLocation,

    // ノードの全体の範囲。例えば `(2 + 3)` のカッコも含めた部分。
    total_loc: SourceLocation,
}

impl Ast {
    pub(crate) fn new(kind: AstKind, children: Vec<Ast>, loc: SourceLocation) -> Self {
        Ast {
            kind,
            children,
            main_loc: loc,
            total_loc: loc,
        }
    }

    pub(crate) fn kind(&self) -> &AstKind {
        &self.kind
    }

    pub(crate) fn children(&self) -> &[Ast] {
        &self.children
    }

    pub(crate) fn loc(&self) -> SourceLocation {
        self.main_loc
    }

    pub(crate) fn total_loc(&self) -> SourceLocation {
        self.total_loc
    }

    // ノードの全体の範囲を拡張する。
    pub(crate) fn extend_loc(&mut self, other_loc: &SourceLocation) {
        self.total_loc = self.total_loc().union(other_loc);
    }
}
