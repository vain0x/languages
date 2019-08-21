use crate::syntax::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Lit {
    Bool(bool),
    Int(i64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Prim {
    Assert,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ExprKind {
    Lit(Lit),
    Prim(Prim),
    Ident(String),
    Fun(usize, String),
    Global(usize, String),
    Call,
    Do,
    Assign,
    If,

    /// `return`
    Ret,

    /// `fn f() { .. }`
    FunDecl {
        fun_id: usize,
    },

    Semi,
}

#[derive(Clone, Debug)]
pub(crate) struct Expr {
    kind: ExprKind,
    children: Vec<Expr>,
    extent: SourceExtent,
}

impl Expr {
    pub(crate) fn new(kind: ExprKind, children: Vec<Expr>, extent: SourceExtent) -> Expr {
        Expr {
            kind,
            children,
            extent,
        }
    }

    pub(crate) fn new_leaf(kind: ExprKind, extent: SourceExtent) -> Expr {
        Expr::new(kind, vec![], extent)
    }

    pub(crate) fn new_bool(value: bool, extent: SourceExtent) -> Expr {
        Expr::new_leaf(ExprKind::Lit(Lit::Bool(value)), extent)
    }

    pub(crate) fn new_int(value: i64, extent: SourceExtent) -> Expr {
        Expr::new_leaf(ExprKind::Lit(Lit::Int(value)), extent)
    }

    pub(crate) fn new_prim(prim: Prim, extent: SourceExtent) -> Expr {
        Expr::new_leaf(ExprKind::Prim(prim), extent)
    }

    pub(crate) fn new_ident(ident: String, extent: SourceExtent) -> Expr {
        Expr::new_leaf(ExprKind::Ident(ident), extent)
    }

    pub(crate) fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub(crate) fn kind_mut(&mut self) -> &mut ExprKind {
        &mut self.kind
    }

    pub(crate) fn into_do(self) -> Expr {
        let extent = *self.extent();
        Expr::new(ExprKind::Do, vec![self], extent)
    }

    pub(crate) fn children(&self) -> &[Expr] {
        &self.children
    }

    pub(crate) fn children_mut(&mut self) -> &mut Vec<Expr> {
        &mut self.children
    }

    pub(crate) fn extent(&self) -> &SourceExtent {
        &self.extent
    }

    pub(crate) fn total_loc(&self) -> &SourceLocation {
        self.extent().total()
    }

    pub(crate) fn is_statement(&self) -> bool {
        match self.kind() {
            ExprKind::Do | ExprKind::If | ExprKind::Assign => true,
            _ => false,
        }
    }

    pub(crate) fn is_decl(&self) -> bool {
        match self.kind() {
            ExprKind::FunDecl { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn short_text<'a>(&self, s: &'a SourceFileSystem) -> (&'a str, bool) {
        let text = s.loc_text(self.total_loc()).trim();
        let mut split = text.split('\n');
        let first_line = split.next().unwrap_or("");
        let omit = split.next().is_some();
        (first_line, omit)
    }
}
