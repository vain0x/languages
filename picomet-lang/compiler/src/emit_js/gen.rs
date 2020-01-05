use super::*;
use crate::pir::*;
use crate::semantics::*;
use crate::syntax::*;

use std::collections::BTreeMap;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VarOrFunId {
    Var(VarId),
    Fun(FunId),
}

struct JsGen {
    vars: BTreeMap<VarOrFunId, JsVarId>,
}

fn op_to_js_op(op: Op) -> JsOp {
    match op {
        Op::Set => JsOp::Set,
        Op::SetAdd => JsOp::SetAdd,
        Op::SetSub => JsOp::SetSub,
        Op::SetMul => JsOp::SetMul,
        Op::SetDiv => JsOp::SetDiv,
        Op::Eq => JsOp::Eq,
        Op::Ne => JsOp::Ne,
        Op::Lt => JsOp::Lt,
        Op::Le => JsOp::Le,
        Op::Gt => JsOp::Gt,
        Op::Ge => JsOp::Ge,
        Op::Add => JsOp::Add,
        Op::Sub => JsOp::Sub,
        Op::BitOr => JsOp::BitOr,
        Op::BitXor => JsOp::BitXor,
        Op::Mul => JsOp::Mul,
        Op::Div => JsOp::Div,
        Op::Mod => JsOp::Mod,
        Op::BitAnd => JsOp::BitAnd,
        Op::BitShiftL => JsOp::BitShiftL,
        Op::BitShiftR => JsOp::BitShiftR,
        _ => unimplemented!(),
    }
}

fn prim_to_js_prim(prim: Prim) -> JsPrim {
    match prim {
        Prim::Print => JsPrim::Print,
        Prim::PrintLnInt => JsPrim::PrintLn,
        _ => unimplemented!(),
    }
}

impl JsGen {
    fn js_var_id_from_var_id(&self, var_id: VarId) -> JsVarId {
        self.vars[&VarOrFunId::Var(var_id)]
    }

    fn js_var_id_from_fun_id(&self, fun_id: FunId) -> JsVarId {
        self.vars[&VarOrFunId::Fun(fun_id)]
    }

    fn gen_exp(&self, pir: &Pir) -> JsExp {
        match pir.kind() {
            PirKind::Val(PirVal::Unit) => JsExp::Val(JsVal::Null),
            &PirKind::Val(PirVal::Byte(value)) => JsExp::Val(JsVal::Num(value as f64)),
            &PirKind::Val(PirVal::Int(value)) => JsExp::Val(JsVal::Num(value as f64)),
            PirKind::Val(PirVal::Str(value)) => JsExp::Val(JsVal::Str(value.to_string())),
            &PirKind::Var { var_id } => JsExp::Var {
                var_id: self.js_var_id_from_var_id(var_id),
            },
            &PirKind::Op { op, .. } => {
                let args = self.gen_exps(pir.children());
                JsExp::Bin {
                    op: op_to_js_op(op),
                    args,
                }
            }
            &PirKind::CallPrim(prim) => {
                let js_prim = prim_to_js_prim(prim);
                let mut args = self.gen_exps(pir.children());
                args.insert(0, JsExp::Val(JsVal::Prim(js_prim)));
                JsExp::Call { args }
            }
            &PirKind::CallFun(fun_id) => {
                let var_id = self.js_var_id_from_fun_id(fun_id);
                let mut args = self.gen_exps(pir.children());
                args.insert(0, JsExp::Var { var_id });
                JsExp::Call { args }
            }
            PirKind::Deref { .. } => self.gen_exp(&pir.children()[0]),
            _ => unimplemented!("{:?}", pir),
        }
    }

    fn gen_exps(&self, children: &[Pir]) -> Vec<JsExp> {
        children.iter().map(|pir| self.gen_exp(pir)).collect()
    }

    fn gen_stm(&self, pir: &Pir, out: &mut Vec<JsStm>) {
        match &pir.kind() {
            PirKind::If => {
                let cond = Box::new(self.gen_exp(&pir.children()[0]));
                let mut body =vec![];
                self.gen_stm(&pir.children()[1], &mut body);
                let mut alt = vec![];
                self.gen_stm(&pir.children()[2], &mut alt);
                out.push(JsStm::If {
                    cond, body, alt
                });
            }
            PirKind::Loop {..} => {
                let mut body =vec![];
                self.gen_stm(&pir.children()[0], &mut body);
                out.push(JsStm::Loop{
                    body,
                });
            }
            PirKind::Break { ..} => {
                out.push(JsStm::Break);
            }
            PirKind::Continue {..} => {
                out.push(JsStm::Continue);
            }
            PirKind::Return => {
                let arg = self.gen_exp(&pir.children()[0]);
                out.push(JsStm::Return(Box::new(arg)));
            }
            PirKind::Semi => {
                for pir in pir.children() {
                    self.gen_stm(pir, out);
                }
            }
            _ => {
                let body = self.gen_exp(pir);
                out.push(JsStm::Exp(body));
            }
        }
    }

    fn gen_fun(&self, fun_id: FunId, fun_def: &PirFunDef) -> JsStm {
        let params = fun_def
            .args()
            .iter()
            .map(|&var_id| self.js_var_id_from_var_id(var_id))
            .collect();
        let mut body = vec![];
        self.gen_stm(fun_def.body(), &mut body);
        let fun_exp = JsExp::Fun { params, body };
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
    for (fun_id, _) in program.funs() {
        let js_var_id = JsVarId::new(js_gen.vars.len());
        js_gen.vars.insert(VarOrFunId::Fun(fun_id), js_var_id);
    }

    let mut stms = vec![];
    for (fun_id, fun_def) in program.funs() {
        let stm = js_gen.gen_fun(fun_id, fun_def);
        stms.push(stm);
    }

    // Entry point.
    stms.push(JsStm::Exp(JsExp::Call {
        args: vec![JsExp::Var {
            var_id: js_gen.js_var_id_from_fun_id(GLOBAL_FUN_ID),
        }],
    }));

    JsProgram { stms }
}
