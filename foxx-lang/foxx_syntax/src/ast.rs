use crate::{
    internals::*,
    parse::{Pos, Range},
    tokenize::Token,
};

#[inline]
fn box_in<'b, T: 'b>(value: T, bump: &'b Bump) -> BumpBox<'b, T> {
    BumpBox::new_in(value, bump)
}

#[inline]
fn opt_box_in<'b, T: 'b>(value_opt: Option<T>, bump: &'b Bump) -> Option<BumpBox<'b, T>> {
    value_opt.map(|value| BumpBox::new_in(value, bump))
}

// -----------------------------------------------
// フラグメント
// -----------------------------------------------

pub struct ALit<'b> {
    pub token: Token,
    pub text: &'b str,
    pub range: Range,
}

pub struct AName<'b> {
    pub text: &'b str,
    pub pos: Pos,
}

pub struct AField<'b> {
    pub name: AName<'b>,
    pub ty: ATy<'b>,
    pub range: Range,
}

pub struct AParamList<'b> {
    pub fields: BumpVec<'b, AField<'b>>,
    pub range: Range,
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub enum ATy<'b> {
    Name(AName<'b>),
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

// -----------------------------------------------
// 式
// -----------------------------------------------

pub struct ACallExpr<'b> {
    pub callee: BoxedExpr<'b>,
    pub args: BumpVec<'b, AExpr<'b>>,
    pub pos: Pos,
}

pub struct ABinaryExpr<'b> {
    pub op: BinaryOp,
    pub l: BoxedExpr<'b>,
    pub r: BoxedExpr<'b>,
    pub pos: Pos,
}

pub struct AReturnExpr<'b> {
    pub arg_opt: Option<BoxedExpr<'b>>,
    pub pos: Pos,
}

pub enum AExpr<'b> {
    Lit(ALit<'b>),
    Name(AName<'b>),
    Call(ACallExpr<'b>),
    Binary(ABinaryExpr<'b>),
    Return(AReturnExpr<'b>),
}

impl<'b> AExpr<'b> {
    pub(crate) fn new_call(
        callee: Self,
        args: BumpVec<'b, AExpr<'b>>,
        pos: Pos,
        bump: &'b Bump,
    ) -> Self {
        Self::Call(ACallExpr {
            callee: box_in(callee, bump),
            args,
            pos,
        })
    }

    pub(crate) fn new_binary(op: BinaryOp, l: Self, r: Self, pos: Pos, bump: &'b Bump) -> Self {
        Self::Binary(ABinaryExpr {
            op,
            l: box_in(l, bump),
            r: box_in(r, bump),
            pos,
        })
    }

    pub(crate) fn new_return(arg_opt: Option<AExpr<'b>>, pos: Pos, bump: &'b Bump) -> Self {
        Self::Return(AReturnExpr {
            arg_opt: opt_box_in(arg_opt, bump),
            pos,
        })
    }
}

type BoxedExpr<'b> = BumpBox<'b, AExpr<'b>>;

pub enum BinaryOp {
    Mul,
    Div,
    Modulo,
    Add,
    Sub,
}

// -----------------------------------------------
// 文
// -----------------------------------------------

pub struct AExprStmt<'b>(pub AExpr<'b>);

pub struct ALetStmt<'b> {
    pub name: AName<'b>,
    pub ty_opt: Option<ATy<'b>>,
    pub init: AExpr<'b>,
    pub pos: Pos,
}

pub struct AFnStmt<'b> {
    pub name: AName<'b>,
    pub param_list: AParamList<'b>,
    pub result_ty_opt: Option<ATy<'b>>,
    pub body: AExpr<'b>,
    pub pos: Pos,
}

pub enum AStmt<'b> {
    Expr(AExprStmt<'b>),
    Let(ALetStmt<'b>),
    Fn(AFnStmt<'b>),
}

impl<'b> AStmt<'b> {
    pub(crate) fn new_expr(expr: AExpr<'b>) -> Self {
        AStmt::Expr(AExprStmt(expr))
    }

    pub(crate) fn new_let(
        name: AName<'b>,
        ty_opt: Option<ATy<'b>>,
        init: AExpr<'b>,
        pos: Pos,
    ) -> Self {
        AStmt::Let(ALetStmt {
            name,
            ty_opt,
            init,
            pos,
        })
    }

    pub(crate) fn new_fn(
        name: AName<'b>,
        param_list: AParamList<'b>,
        result_ty_opt: Option<ATy<'b>>,
        body: AExpr<'b>,
        pos: Pos,
    ) -> Self {
        AStmt::Fn(AFnStmt {
            name,
            param_list,
            result_ty_opt,
            body,
            pos,
        })
    }
}

// -----------------------------------------------
// ルート
// -----------------------------------------------

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
