use crate::*;
use std::collections::BTreeSet;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ExpMode {
    Val,
    Ref,
    Pat,
}

pub struct SemanticAnalyzer {
    sema: Sema,
    env: BTreeMap<String, SymbolId>,
    current_fun_id: FunId,
}

impl SemanticAnalyzer {
    fn add_err(&mut self, message: String, exp_id: ExpId) {
        let msg_id = MsgId(self.sema.msgs.len());
        self.sema.msgs.insert(msg_id, Msg::err(message, exp_id));
    }

    fn add_fun(&mut self, name: String, exp_id: ExpId) -> FunId {
        let fun_id = FunId(self.sema.funs.len());
        let fun_def = FunDef {
            name,
            body: exp_id,
            locals: vec![],
        };
        self.sema.funs.insert(fun_id, fun_def);
        fun_id
    }

    fn add_local(&mut self, name: String) -> SymbolId {
        let index = self.sema.funs[&self.current_fun_id].locals.len();
        let symbol_id = SymbolId(self.sema.symbols.len());

        let symbol = Symbol {
            kind: SymbolKind::Local { index },
            name,
        };

        self.sema.symbols.insert(symbol_id, symbol);
        self.current_fun_mut().locals.push(symbol_id);

        symbol_id
    }

    fn set_ty(&mut self, exp_id: ExpId, l_ty: &Ty, r_ty: &Ty) {
        self.sema.unify_ty(exp_id, &Ty::Var(exp_id), l_ty);
        self.sema.unify_ty(exp_id, l_ty, r_ty);
    }

    fn current_fun_mut(&mut self) -> &mut FunDef {
        self.sema.funs.get_mut(&self.current_fun_id).unwrap()
    }

    fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.sema.syntax.exps[&exp_id]
    }

    fn on_pat(&mut self, exp_id: ExpId, ty: Ty) {
        self.on_exp(exp_id, (ExpMode::Pat, ty));
    }

    fn on_ref(&mut self, exp_id: ExpId, ty: Ty) {
        self.on_exp(exp_id, (ExpMode::Ref, ty));
    }

    fn on_val(&mut self, exp_id: ExpId, ty: Ty) {
        self.on_exp(exp_id, (ExpMode::Val, ty));
    }

    fn on_ident_pat(&mut self, exp_id: ExpId, ty: Ty, name: &str) {
        let symbol_id = self.add_local(name.to_string());
        self.env.insert(name.to_string(), symbol_id);
        self.sema.exp_symbols.insert(exp_id, symbol_id);

        self.set_ty(exp_id, &ty, &ty);
    }

    fn on_ident_ref_or_val(&mut self, exp_id: ExpId, mode: ExpMode, ty: Ty, name: &str) {
        let symbol_id = match self.env.get(name) {
            Some(&symbol_id) => symbol_id,
            None => {
                self.add_err("Undefined name".to_string(), exp_id);
                return;
            }
        };

        self.sema.exp_symbols.insert(exp_id, symbol_id);

        match (&self.sema.symbols[&symbol_id].kind, mode) {
            (_, ExpMode::Pat) => unreachable!(),
            (SymbolKind::Prim(prim), ExpMode::Val) => {
                self.set_ty(exp_id, &ty, &prim.get_ty());
            }
            (SymbolKind::Prim { .. }, ExpMode::Ref) => unimplemented!(),
            (SymbolKind::Local { .. }, ExpMode::Val) => {
                self.sema.exp_vals.insert(exp_id);
                self.set_ty(exp_id, &ty, &ty);
            }
            (SymbolKind::Local { .. }, ExpMode::Ref) => {
                self.set_ty(exp_id, &ty, &ty);
            }
        }
    }

    fn sema(&mut self) {
        for &(name, prim) in PRIMS {
            let symbol_id = SymbolId(self.sema.symbols.len());
            self.sema.symbols.insert(
                symbol_id,
                Symbol {
                    kind: SymbolKind::Prim(prim),
                    name: name.to_string(),
                },
            );
            self.env.insert(name.to_string(), symbol_id);
        }

        let root_exp_id = self.sema.syntax.root_exp_id;
        let fun_id = self.add_fun("main".to_string(), root_exp_id);
        assert_eq!(fun_id, GLOBAL_FUN_ID);

        self.current_fun_id = GLOBAL_FUN_ID;
        self.on_val(root_exp_id, Ty::Unit);
    }
}

impl ShareSyntax for SemanticAnalyzer {
    fn share_syntax(&self) -> Rc<Syntax> {
        self.sema.syntax.clone()
    }
}

impl ExpVisitor for SemanticAnalyzer {
    type Mode = (ExpMode, Ty);
    type Output = ();

    fn on_err(&mut self, _: ExpId, _: (ExpMode, Ty), _: MsgId) {}

    fn on_int(&mut self, exp_id: ExpId, (mode, ty): (ExpMode, Ty), _: i64) {
        match mode {
            ExpMode::Pat | ExpMode::Ref => unimplemented!(),
            ExpMode::Val => {}
        }

        self.set_ty(exp_id, &ty, &Ty::Int);
    }

    fn on_str(&mut self, exp_id: ExpId, (mode, ty): (ExpMode, Ty), _: &str) {
        match mode {
            ExpMode::Pat | ExpMode::Ref => unimplemented!(),
            ExpMode::Val => {}
        }
        self.set_ty(exp_id, &ty, &Ty::make_str());
    }

    fn on_ident(&mut self, exp_id: ExpId, (mode, ty): (ExpMode, Ty), name: &str) {
        match mode {
            ExpMode::Pat => self.on_ident_pat(exp_id, ty, name),
            ExpMode::Ref | ExpMode::Val => self.on_ident_ref_or_val(exp_id, mode, ty, name),
        }
    }

    fn on_call(&mut self, exp_id: ExpId, (_, ty): (ExpMode, Ty), callee: ExpId, args: &[ExpId]) {
        self.set_ty(exp_id, &ty, &Ty::Var(exp_id));
        let callee_ty = Ty::make_fun(args.iter().cloned().map(Ty::Var), ty);

        self.on_val(callee, callee_ty);
        for &arg in args {
            self.on_val(arg, Ty::Var(arg));
        }
    }

    fn on_index(&mut self, exp_id: ExpId, (mode, ty): (ExpMode, Ty), indexee: ExpId, arg: ExpId) {
        self.on_val(indexee, Ty::Ptr);
        self.on_val(arg, Ty::Int);
        self.set_ty(exp_id, &ty, &Ty::Byte);

        match mode {
            ExpMode::Pat => panic!("no index pattern"),
            ExpMode::Ref => {}
            ExpMode::Val => {
                self.sema.exp_vals.insert(exp_id);
            }
        }
    }

    fn on_bin(
        &mut self,
        exp_id: ExpId,
        (_, ty): (ExpMode, Ty),
        op: Op,
        exp_l: ExpId,
        exp_r: ExpId,
    ) {
        match op {
            Op::Set | Op::SetAdd => {
                self.on_ref(exp_l, Ty::Var(exp_l));
                self.on_val(exp_r, Ty::Var(exp_l));
                self.set_ty(exp_id, &ty, &Ty::Unit);
                match self.sema.subst_ty(&Ty::Var(exp_l)) {
                    Ty::Byte | Ty::Int | Ty::Ptr => {}
                    ty => self.add_err(
                        format!("You cannot assign non-byte/int/ptr value for now {:?}", ty),
                        exp_id,
                    ),
                }
            }
            _ => {
                self.on_val(exp_l, Ty::Int);
                self.on_val(exp_r, Ty::Int);
                self.set_ty(exp_id, &ty, &Ty::Int);
            }
        }
    }

    fn on_if(
        &mut self,
        exp_id: ExpId,
        (_, ty): (ExpMode, Ty),
        cond: ExpId,
        body: ExpId,
        alt: ExpId,
    ) {
        self.on_val(cond, Ty::Int);
        self.on_val(body, Ty::Var(exp_id));
        self.on_val(alt, Ty::Var(exp_id));
        self.set_ty(exp_id, &Ty::Var(exp_id), &ty);
    }

    fn on_while(&mut self, exp_id: ExpId, (_, ty): (ExpMode, Ty), cond: ExpId, body: ExpId) {
        self.on_val(cond, Ty::Int);
        self.on_val(body, Ty::Unit);
        self.set_ty(exp_id, &ty, &Ty::Unit);
    }

    fn on_let(&mut self, exp_id: ExpId, (_, ty): (ExpMode, Ty), pat: ExpId, init: ExpId) {
        self.on_pat(pat, Ty::Var(pat));
        self.on_val(init, Ty::Var(pat));
        self.set_ty(exp_id, &ty, &Ty::Unit)
    }

    fn on_semi(&mut self, exp_id: ExpId, (_, ty): (ExpMode, Ty), exps: &[ExpId]) {
        for &exp_id in exps {
            self.on_val(exp_id, Ty::Var(exp_id));
        }

        let last_ty = exps.last().cloned().map(Ty::Var).unwrap_or(Ty::Unit);
        self.set_ty(exp_id, &ty, &last_ty);
    }
}

impl Sema {
    fn add_err(&mut self, message: String, exp_id: ExpId) {
        let msg_id = MsgId(self.msgs.len());
        self.msgs.insert(msg_id, Msg::err(message, exp_id));
    }

    pub fn get_ty(&self, exp_id: ExpId) -> Ty {
        self.subst_ty(&Ty::Var(exp_id)).to_owned()
    }

    fn bind_ty(&mut self, exp_id: ExpId, ty: &Ty) {
        self.exp_tys.insert(exp_id, ty.to_owned());
    }

    fn subst_ty<'a>(&'a self, ty: &'a Ty) -> &'a Ty {
        match ty {
            Ty::Var(exp_id) => match self.exp_tys.get(&exp_id) {
                Some(ty) => ty,
                None => ty,
            },
            _ => ty,
        }
    }

    fn unify_ty(&mut self, exp_id: ExpId, l_ty: &Ty, r_ty: &Ty) {
        let subst_l_ty = self.subst_ty(l_ty).to_owned();
        let subst_r_ty = self.subst_ty(r_ty).to_owned();
        match (&subst_l_ty, &subst_r_ty) {
            (Ty::Var(l_exp_id), Ty::Var(r_exp_id)) if l_exp_id == r_exp_id => {}
            (&Ty::Var(exp_id), _) | (_, &Ty::Var(exp_id)) => self.bind_ty(exp_id, &subst_r_ty),
            (Ty::Err, _)
            | (_, Ty::Err)
            | (Ty::Unit, Ty::Unit)
            | (Ty::Byte, Ty::Byte)
            | (Ty::Int, Ty::Int)
            | (Ty::Ptr, Ty::Ptr) => {}
            (Ty::Fun(l_tys), Ty::Fun(r_tys)) => self.unify_tys(exp_id, &l_tys, &r_tys),
            (Ty::Unit, _) | (Ty::Int, _) | (Ty::Byte, _) | (Ty::Ptr, _) | (Ty::Fun(_), _) => {
                eprintln!("Type Error left={:?} right={:?}", subst_l_ty, subst_r_ty);
                self.add_err("Type Error".to_string(), exp_id)
            }
        }
    }

    fn unify_tys(&mut self, exp_id: ExpId, l_tys: &[Ty], r_tys: &[Ty]) {
        if l_tys.len() != r_tys.len() {
            self.add_err("Type Error".to_string(), exp_id);
            return;
        }

        for (l_ty, r_ty) in l_tys.iter().zip(r_tys.iter()) {
            self.unify_ty(exp_id, l_ty, r_ty);
        }
    }
}

pub fn sema(syntax: Rc<Syntax>) -> Sema {
    let mut analyzer = SemanticAnalyzer {
        sema: Sema {
            syntax: Rc::clone(&syntax),
            symbols: BTreeMap::new(),
            exp_symbols: BTreeMap::new(),
            exp_vals: BTreeSet::new(),
            exp_tys: BTreeMap::new(),
            funs: BTreeMap::new(),
            msgs: syntax.msgs.clone(),
        },
        env: BTreeMap::new(),
        current_fun_id: GLOBAL_FUN_ID,
    };
    analyzer.sema();
    analyzer.sema
}
