use crate::semantics::VarId;

mod gen;
mod print;

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
        var_id: VarId,
    },
    Prim {
        prim: JsPrim,
        args: Vec<JsExp>,
    },
    Fun {
        params: Vec<String>,
        body: Vec<JsStm>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum JsStm {
    Let { pat: String, body: JsExp },
}

#[derive(Clone, Debug)]
pub(crate) struct JsProgram {
    stms: Vec<JsStm>,
}
