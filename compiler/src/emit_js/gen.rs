use super::*;
use crate::pir::*;

pub(crate) fn js_gen() -> JsProgram {
    JsProgram {
        stms: vec![JsStm::Let {
            pat: "main".to_string(),
            body: JsExp::Fun {
                params: vec![],
                body: vec![JsStm::Let {
                    pat: "_".to_string(),
                    body: JsExp::Prim {
                        prim: JsPrim::PrintLn,
                        args: vec![JsExp::Val(JsVal::Num(42.0))],
                    },
                }],
            },
        }],
    }
}
