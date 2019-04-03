use super::*;
use std::rc::Rc;

struct GenPir {
    sema: Rc<Sema>,
    program: PirProgram,
}

impl GenPir {
    fn sema(&self) -> &Sema {
        &self.sema
    }

    fn share_sema(&self) -> Rc<Sema> {
        Rc::clone(&self.sema)
    }

    fn pir(&self, kind: PirKind, args: Vec<Pir>, exp_id: ExpId) -> Pir {
        Pir {
            kind,
            args,
            ty: self.sema().exp_ty(exp_id),
            exp_id,
        }
    }

    fn on_ident(&mut self, exp_id: ExpId, _: &str) -> Pir {
        let symbol_ref = (self.sema().exp_as_symbol(exp_id)).expect("Missing ident symbol");
        match symbol_ref {
            SymbolRef::Prim(..) => panic!("cannot generate primitive"),
            SymbolRef::Var(var_id, _) => {
                let pir = self.pir(PirKind::Var { var_id }, vec![], exp_id);

                if self.sema().exp_is_coerced_to_value(exp_id) {
                    pir.into_deref()
                } else {
                    pir
                }
            }
        }
    }

    fn on_call(&mut self, exp_id: ExpId, callee: ExpId, args: &[ExpId]) -> Pir {
        let symbol_ref = (self.sema().exp_as_symbol(callee)).expect("Can't call non-symbol");
        match symbol_ref {
            SymbolRef::Prim(prim) => {
                let args = args.into_iter().map(|&arg| self.on_exp(arg)).collect();
                self.pir(PirKind::CallPrim(prim), args, exp_id)
            }
            SymbolRef::Var(
                _,
                &VarDef {
                    kind: VarKind::Fun(fun_id),
                    ..
                },
            ) => {
                let args = args.into_iter().map(|&arg| self.on_exp(arg)).collect();
                self.pir(PirKind::CallFun(fun_id), args, exp_id)
            }
            SymbolRef::Var(..) => panic!("cannot call on non-primitive/function value"),
        }
    }

    fn on_bin(&mut self, exp_id: ExpId, op: Op, exp_l: ExpId, exp_r: ExpId) -> Pir {
        let l = self.on_exp(exp_l);
        let r = self.on_exp(exp_r);
        self.pir(PirKind::Op { op, size: 0 }, vec![l, r], exp_id)
    }

    fn on_exp(&mut self, exp_id: ExpId) -> Pir {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            ExpKind::Err(err) => panic!("Can't perform code generation if error {:?}", err),
            &ExpKind::Unit => self.pir(PirKind::unit(), vec![], exp_id),
            &ExpKind::Int(value) => self.pir(PirKind::int(value), vec![], exp_id),
            &ExpKind::Byte(value) => self.pir(PirKind::byte(value), vec![], exp_id),
            ExpKind::Str(value) => self.pir(PirKind::str(value.clone()), vec![], exp_id),
            ExpKind::Ident(name) => self.on_ident(exp_id, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, *callee, args),
            &ExpKind::Index { indexee, arg } => {
                let indexee = self.on_exp(indexee);

                if let Some((l, r)) = self.sema().exp_as_range(arg) {
                    let l = self.on_exp(l);
                    let r = self.on_exp(r);
                    return self.pir(PirKind::IndexSlice, vec![indexee, l, r], exp_id);
                }

                let arg = self.on_exp(arg);
                let result = self.pir(PirKind::IndexPoint, vec![indexee, arg], exp_id);

                if self.sema().exp_is_coerced_to_value(exp_id) {
                    result.into_deref()
                } else {
                    result
                }
            }
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, op, l, r),
            ExpKind::Fun { .. } => panic!("Can't generate fun"),
            &ExpKind::Return(result) => {
                let result = self.on_exp(result);
                self.pir(PirKind::Return, vec![result], exp_id)
            }
            &ExpKind::If { cond, body, alt } => {
                let cond = self.on_exp(cond);
                let body = self.on_exp(body);
                let alt = self.on_exp(alt);
                self.pir(PirKind::If, vec![cond, body, alt], exp_id)
            }
            &ExpKind::While { cond, body } => {
                let loop_id = (self.sema().exp_as_loop(exp_id)).expect("Missing loop on continue");
                let cond = self.on_exp(cond);
                let body = self.on_exp(body);
                self.pir(PirKind::While { loop_id }, vec![cond, body], exp_id)
            }
            &ExpKind::Break => {
                let loop_id = (self.sema().exp_as_loop(exp_id)).expect("Missing loop id on break");
                self.pir(PirKind::Break { loop_id }, vec![], exp_id)
            }
            &ExpKind::Continue => {
                let loop_id =
                    (self.sema().exp_as_loop(exp_id)).expect("Missing loop id on continue");
                self.pir(PirKind::Continue { loop_id }, vec![], exp_id)
            }
            &ExpKind::Let { pat, init, .. } => {
                if self.sema().exp_is_decl(exp_id) {
                    return self.pir(PirKind::unit(), vec![], exp_id);
                }

                let init = self.on_exp(init);
                let var = self.on_exp(pat);
                self.pir(
                    PirKind::Op {
                        op: Op::Set,
                        size: 0,
                    },
                    vec![var, init],
                    exp_id,
                )
            }
            ExpKind::Semi(exps) => self.on_exps(exps, exp_id),
        }
    }

    fn on_exps(&mut self, exps: &[ExpId], exp_id: ExpId) -> Pir {
        let pirs = exps
            .iter()
            .map(|&exp_id| self.on_exp(exp_id))
            .collect::<Vec<_>>();
        let ty = pirs.last().map(|pir| pir.ty()).unwrap_or(Ty::unit());

        Pir {
            kind: PirKind::Semi,
            args: pirs,
            ty,
            exp_id,
        }
    }

    pub fn gen_fun(&mut self, fun_id: FunId) {
        let fun_def = &self.share_sema().funs[&fun_id];

        let exp_id = fun_def.bodies().last().cloned().unwrap();
        let body = self.on_exps(&fun_def.bodies(), exp_id);

        let args = self.sema().fun_arg_ids(fun_id).collect();
        let locals = self.sema().fun_local_ids(fun_id).collect();

        self.program.funs.insert(
            fun_id,
            PirFunDef {
                args,
                locals,
                body,
                exp_id,
                is_global: fun_id == GLOBAL_FUN_ID,
            },
        );
    }

    fn import_vars(&mut self) {
        for &var_id in self.share_sema().vars.keys() {
            let var_def = &self.sema().vars[&var_id];
            let var_def = PirVarDef {
                is_temporary: false,
                exp_id: var_def.def_exp_id,
            };
            self.program.vars.insert(var_id, var_def);
        }
    }

    pub fn gen_pir(&mut self) {
        self.import_vars();

        for &fun_id in self.share_sema().funs.keys() {
            self.gen_fun(fun_id);
        }
    }
}

impl ShareSyntax for GenPir {
    fn share_syntax(&self) -> Rc<Syntax> {
        Rc::clone(&self.sema().syntax)
    }
}

pub(crate) fn gen(sema: Rc<Sema>) -> PirProgram {
    let mut gen_pir = GenPir {
        sema,
        program: PirProgram {
            vars: BTreeMap::new(),
            funs: BTreeMap::new(),
        },
    };
    gen_pir.gen_pir();
    gen_pir.program
}
