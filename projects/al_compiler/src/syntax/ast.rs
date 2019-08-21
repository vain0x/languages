//! 抽象構文木 (abstract syntax tree)
//!
//! ソースコードから瑣末な情報 (スペースや優先順位のためのカッコなど) を取り除いて、
//! 式の構造を表現したもの。

use crate::syntax::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum AstKind {
    True,
    False,
    Ident(String),
    Int(i64),
    Assert,
    If,

    /// `fn f() { .. }`
    FunDecl,

    Bin(BinOp),
    Call,
    Semi,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Ast {
    kind: AstKind,
    children: Vec<Ast>,
    extent: SourceExtent,
}

impl Ast {
    pub(crate) fn new(kind: AstKind, children: Vec<Ast>, extent: SourceExtent) -> Self {
        Ast {
            kind,
            children,
            extent,
        }
    }

    pub(crate) fn kind(&self) -> &AstKind {
        &self.kind
    }

    pub(crate) fn children(&self) -> &[Ast] {
        &self.children
    }

    pub(crate) fn extent(&self) -> &SourceExtent {
        &self.extent
    }

    // ノードの全体の範囲を拡張する。
    pub(crate) fn extend_loc(&mut self, loc: &SourceLocation) {
        self.extent.extend(loc)
    }
}
