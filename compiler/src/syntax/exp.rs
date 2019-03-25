use super::*;
use std::iter;

/// Expression in concrete syntax tree.
#[derive(Clone, Debug)]
pub(crate) enum ExpKind {
    Err(SyntaxError),
    Unit,
    Int(i64),
    Byte(u8),
    Str(String),
    Ident(String),
    Call {
        callee: ExpId,
        args: Vec<ExpId>,
    },
    Index {
        indexee: ExpId,
        arg: ExpId,
    },
    Bin {
        op: Op,
        l: ExpId,
        r: ExpId,
    },
    Fun {
        pats: Vec<ExpId>,
        body: ExpId,
    },
    Return(ExpId),
    If {
        cond: ExpId,
        body: ExpId,
        alt: ExpId,
    },
    While {
        cond: ExpId,
        body: ExpId,
    },
    Break,
    Continue,
    Let {
        pat: ExpId,
        init: ExpId,
        rec: bool,
    },
    Semi(Vec<ExpId>),
}

#[derive(Clone, Debug)]
pub(crate) struct Exp {
    pub kind: ExpKind,
    pub module_id: ModuleId,
    pub span: Span,
}

impl ExpKind {
    pub(crate) fn is_stmt(&self) -> bool {
        match self {
            ExpKind::Return(..)
            | ExpKind::While { .. }
            | ExpKind::Break
            | ExpKind::Continue
            | ExpKind::Let { .. }
            | ExpKind::Semi(_) => true,
            _ => false,
        }
    }

    pub(crate) fn children(&self) -> Vec<ExpId> {
        match self {
            ExpKind::Err(_)
            | ExpKind::Unit
            | ExpKind::Int(_)
            | ExpKind::Byte(_)
            | ExpKind::Str(_)
            | ExpKind::Ident(_)
            | ExpKind::Break
            | ExpKind::Continue => vec![],
            ExpKind::Call { callee, args } => {
                iter::once(callee).chain(args.iter()).cloned().collect()
            }
            &ExpKind::Index { indexee, arg } => vec![indexee, arg],
            &ExpKind::Bin { l, r, .. } => vec![l, r],
            ExpKind::Fun { pats, body } => pats.iter().chain(iter::once(body)).cloned().collect(),
            &ExpKind::Return(exp_id) => vec![exp_id],
            &ExpKind::If { cond, body, alt } => vec![cond, body, alt],
            &ExpKind::While { cond, body } => vec![cond, body],
            &ExpKind::Let { pat, init, .. } => vec![pat, init],
            ExpKind::Semi(exps) => exps.to_owned(),
        }
    }
}
