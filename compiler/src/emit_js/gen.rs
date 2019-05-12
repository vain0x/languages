use super::*;
use crate::pir::*;
use crate::syntax::*;
use crate::semantics::*;
use crate::pir::*;
use std::collections::BTreeMap;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VarOrFunId {
    Var(VarId),
    Fun(FunId),
}

struct JsGen {
    vars: BTreeMap<VarOrFunId, JsVarId>,
}

impl JsGen {
    fn js_var_id_from_var_id(&self, var_id: VarId) -> JsVarId {
        self.vars[&VarOrFunId::Var(var_id)]
    }

    fn js_var_id_from_fun_id(&self, fun_id: FunId) -> JsVarId {
        self.vars[&VarOrFunId::Fun(fun_id)]
    }

    fn gen_exp(&self, pir: &Pir) -> JsExp {
        match &pir.kind() {
            PirKind::Op { op: Op::Add, .. } => {
                let args = self.gen_exps(pir.children());
JsExp::Prim {
                    prim: JsPrim::Add,
                    args,
                }
            }
            _ => unimplemented!()
        }
    }

    fn gen_exps(&self, children: &[Pir]) -> Vec<JsExp> {
        children.iter().map(|pir| self.gen_exp(pir)).collect()
    }

    fn gen_stm(&self, pir: &Pir, out: &mut Vec<JsStm>) {
        match &pir.kind() {
            PirKind::Op { op: Op::Set, .. } => {
                let pat = self.js_var_id_from_var_id(pir.children()[0].as_var().unwrap());
                let body = self.gen_exp(&pir.children()[1]);
                out.push(JsStm::Let {
                    pat,
                    body,
                });
            }
            PirKind::Op { .. } => {
                let body = self.gen_exp(pir);
                out.push(JsStm::Exp(body));
            }
            PirKind::Semi => {
                for pir in pir.children() {
                    self.gen_stm(pir, out);
                }
            }
            _ => unimplemented!()
        }
    }

    fn gen_fun(&self, fun_id: FunId, fun_def: &PirFunDef) -> JsStm {
        let params =  fun_def.args().iter().map(|&var_id| self.js_var_id_from_var_id(var_id)).collect();
        let mut body = vec![];
        self.gen_stm(fun_def.body(), &mut body);
        let fun_exp = JsExp::Fun {
            params,
            body,
        };
        JsStm::Let {
            pat: self.js_var_id_from_fun_id(fun_id),
            body: fun_exp,
        }
    }
}

pub(crate) fn js_gen(program: &PirProgram) -> JsProgram {
    let mut js_gen = JsGen {
        vars: BTreeMap::new(),
    };

    for (var_id, _) in program.vars() {
        let js_var_id = JsVarId::new(js_gen.vars.len());
        js_gen.vars.insert(VarOrFunId::Var(var_id), js_var_id);
    }
    for (fun_id, fun_def) in program.funs() {
        let js_var_id = JsVarId::new(js_gen.vars.len());
        js_gen.vars.insert(VarOrFunId::Fun(fun_id), js_var_id);
    }

    let mut stms = vec![];
    for (fun_id, fun_def) in program.funs() {
        let stm = js_gen.gen_fun(fun_id, fun_def);
        stms.push(stm);
    }

    JsProgram {
        stms,
    }
}
