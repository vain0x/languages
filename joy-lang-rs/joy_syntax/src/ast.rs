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
pub struct AExprStmt<'b>(pub AExpr<'b>);

#[derive(Debug)]
pub struct ALetStmt<'b> {
    pub name: AName<'b>,
    pub init: AExpr<'b>,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct AFnStmt<'b> {
    pub name: AName<'b>,
    pub body: AExpr<'b>,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum AStmt<'b> {
    Expr(AExprStmt<'b>),
    Let(ALetStmt<'b>),
    Fn(AFnStmt<'b>),
}

impl<'b> AStmt<'b> {
    pub(crate) fn new_expr(expr: AExpr<'b>) -> Self {
        AStmt::Expr(AExprStmt(expr))
    }

    pub(crate) fn new_let(name: AName<'b>, init: AExpr<'b>, pos: Pos) -> Self {
        AStmt::Let(ALetStmt { name, init, pos })
    }

    pub(crate) fn new_fn(name: AName<'b>, body: AExpr<'b>, pos: Pos) -> Self {
        AStmt::Fn(AFnStmt { name, body, pos })
    }
}

#[derive(Debug)]
pub struct ARoot<'b> {
    pub stmts: BumpVec<'b, AStmt<'b>>,
}

impl<'b> ARoot<'b> {
    pub fn new(bump: &'b Bump) -> Self {
        Self {
            stmts: bumpalo::vec![in bump],
        }
    }

    pub fn push(&mut self, decl: AStmt<'b>) {
        self.stmts.push(decl);
    }
}
