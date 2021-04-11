use crate::{internals::*, parse::Pos, tokenize::Token};

#[derive(Debug)]
pub struct ALit<'b> {
    pub token: Token,
    pub text: &'b str,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct AName<'b> {
    pub text: &'b str,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct ACallExpr<'b> {
    pub callee: BoxedExpr<'b>,
    pub args: BumpVec<'b, AExpr<'b>>,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct ABinaryExpr<'b> {
    pub op: BinaryOp,
    pub l: BoxedExpr<'b>,
    pub r: BoxedExpr<'b>,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum AExpr<'b> {
    Lit(ALit<'b>),
    Name(AName<'b>),
    Call(ACallExpr<'b>),
    Binary(ABinaryExpr<'b>),
}

impl<'b> AExpr<'b> {
    pub(crate) fn boxed_in(self, bump: &'b Bump) -> BoxedExpr<'b> {
        BumpBox::new_in(self, bump)
    }

    pub(crate) fn new_call(
        callee: Self,
        args: BumpVec<'b, AExpr<'b>>,
        pos: Pos,
        bump: &'b Bump,
    ) -> Self {
        Self::Call(ACallExpr {
            callee: callee.boxed_in(bump),
            args,
            pos,
        })
    }

    pub(crate) fn new_binary(op: BinaryOp, l: Self, r: Self, pos: Pos, bump: &'b Bump) -> Self {
        Self::Binary(ABinaryExpr {
            op,
            l: l.boxed_in(bump),
            r: r.boxed_in(bump),
            pos,
        })
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
pub struct ALetDecl<'b> {
    pub name: AName<'b>,
    pub init: AExpr<'b>,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct AFnDecl<'b> {
    pub name: AName<'b>,
    pub body: AExpr<'b>,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum ADecl<'b> {
    Expr(AExprDecl<'b>),
    Let(ALetDecl<'b>),
    Fn(AFnDecl<'b>),
}

impl<'b> ADecl<'b> {
    pub(crate) fn new_expr(expr: AExpr<'b>) -> Self {
        ADecl::Expr(AExprDecl(expr))
    }

    pub(crate) fn new_let(name: AName<'b>, init: AExpr<'b>, pos: Pos) -> Self {
        ADecl::Let(ALetDecl { name, init, pos })
    }

    pub(crate) fn new_fn(name: AName<'b>, body: AExpr<'b>, pos: Pos) -> Self {
        ADecl::Fn(AFnDecl { name, body, pos })
    }
}

#[derive(Debug)]
pub struct ARoot<'b> {
    pub decls: BumpVec<'b, ADecl<'b>>,
}

impl<'b> ARoot<'b> {
    pub fn new(bump: &'b Bump) -> Self {
        Self {
            decls: bumpalo::vec![in bump],
        }
    }

    pub fn push(&mut self, decl: ADecl<'b>) {
        self.decls.push(decl);
    }
}
