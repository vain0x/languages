use crate::semantics::VarId;

mod gen;
pub(crate) mod print;

pub(crate) type JsVarId = crate::Id<JsVar>;

pub(crate) struct JsVar;

#[derive(Clone, Copy, Debug)]
pub(crate) enum JsPrim {
    Add,
    PrintLn,
}

#[derive(Clone, Debug)]
pub(crate) enum JsVal {
    Null,
    Num(f64),
    Str(String),
}

#[derive(Clone, Debug)]
pub(crate) enum JsExp {
    Val(JsVal),
    Var {
        var_id: JsVarId,
    },
    Prim {
        prim: JsPrim,
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
    Let { pat: JsVarId, body: JsExp },
}

#[derive(Clone, Debug)]
pub(crate) struct JsProgram {
    stms: Vec<JsStm>,
}
