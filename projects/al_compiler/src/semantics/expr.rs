use crate::syntax::*;

#[derive(Clone, Copy, Debug)]
pub(crate) enum Lit {
    Bool(bool),
    Int(i64),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Prim {
    Assert,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
}

#[derive(Clone, Debug)]
pub(crate) enum ExprKind {
    Lit(Lit),
    Prim(Prim),
    Ident(String),
    Call,
    Semi,
}

#[derive(Clone, Debug)]
pub(crate) struct Expr {
    kind: ExprKind,
    children: Vec<Expr>,
    main_loc: SourceLocation,
    wide_loc: SourceLocation,
}

impl Expr {
    pub(crate) fn new(
        kind: ExprKind,
        children: Vec<Expr>,
        main_loc: SourceLocation,
        wide_loc: SourceLocation,
    ) -> Expr {
        Expr {
            kind,
            children,
            main_loc,
            wide_loc,
        }
    }

    pub(crate) fn new_leaf(kind: ExprKind, loc: SourceLocation) -> Expr {
        Expr::new(kind, vec![], loc, loc)
    }

    pub(crate) fn new_bool(value: bool, loc: SourceLocation) -> Expr {
        Expr::new_leaf(ExprKind::Lit(Lit::Bool(value)), loc)
    }

    pub(crate) fn new_int(value: i64, loc: SourceLocation) -> Expr {
        Expr::new_leaf(ExprKind::Lit(Lit::Int(value)), loc)
    }

    pub(crate) fn new_prim(prim: Prim, loc: SourceLocation) -> Expr {
        Expr::new_leaf(ExprKind::Prim(prim), loc)
    }

    pub(crate) fn new_ident(ident: String, loc: SourceLocation) -> Expr {
        Expr::new_leaf(ExprKind::Ident(ident), loc)
    }

    pub(crate) fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub(crate) fn children(&self) -> &[Expr] {
        &self.children
    }

    // pub(crate) fn main_loc(&self) -> SourceLocation {
    //     self.main_loc
    // }

    pub(crate) fn wide_loc(&self) -> &SourceLocation {
        &self.wide_loc
    }

    pub(crate) fn short_text<'a>(&self, s: &'a SourceFileSystem) -> (&'a str, bool) {
        let text = s.loc_text(self.wide_loc()).trim();
        let mut split = text.split('\n');
        let first_line = split.next().unwrap_or("");
        let omit = split.next().is_some();
        (first_line, omit)
    }
}
