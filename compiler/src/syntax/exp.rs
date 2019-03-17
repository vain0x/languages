use super::*;

/// Expression in concrete syntax tree.
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ExpKind {
    Err(String),
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
