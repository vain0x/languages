use crate::semantics::VarId;

mod gen;
pub(crate) mod print;

pub(crate) type JsVarId = crate::Id<JsVar>;

pub(crate) struct JsVar;

#[derive(Clone, Copy, Debug)]
pub(crate) enum JsOp {
    Set,
    SetAdd,
    SetSub,
    SetMul,
    SetDiv,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    BitOr,
    BitXor,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitShiftL,
    BitShiftR,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum JsPrim {
    Print,
    PrintLn,
}

#[derive(Clone, Debug)]
pub(crate) enum JsVal {
    Null,
    Num(f64),
    Str(String),
    Prim(JsPrim),
}

#[derive(Clone, Debug)]
pub(crate) enum JsExp {
    Val(JsVal),
    Var {
        var_id: JsVarId,
    },
    Bin {
        op: JsOp,
        args: Vec<JsExp>,
    },
    Call {
        args: Vec<JsExp>,
    },
    Fun {
        params: Vec<JsVarId>,
        body: Vec<JsStm>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum JsStm {
    Exp(JsExp),
    Let {
        pat: JsVarId,
        body: JsExp,
    },
    If {
        cond: Box<JsExp>,
        body: Vec<JsStm>,
        alt: Vec<JsStm>,
    },
    Loop {
        body: Vec<JsStm>,
    },
    Break,
    Continue,
    Return(Box<JsExp>),
}

#[derive(Clone, Debug)]
pub(crate) struct JsProgram {
    stms: Vec<JsStm>,
}
