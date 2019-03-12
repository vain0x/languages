use crate::*;
use std::collections::BTreeSet;

pub(crate) struct SemanticAnalyzer {
    sema: Sema,
    current_fun_id: FunId,
}

impl SemanticAnalyzer {
    fn current_fun_mut(&mut self) -> &mut FunDef {
        self.sema.funs.get_mut(&self.current_fun_id).unwrap()
    }

    fn imported_symbols(&self) -> impl Iterator<Item = SymbolRef<'_>> {
        (self.sema.fun_symbols(self.current_fun_id).into_iter().rev())
            .chain(self.sema.fun_symbols(GLOBAL_FUN_ID).into_iter().rev())
    }

    fn lookup_symbol(&self, name: &str) -> Option<SymbolRef<'_>> {
        self.imported_symbols().find(|symbol| symbol.name() == name)
    }

    fn add_err(&mut self, message: String, exp_id: ExpId) {
        self.sema.add_err_msg(message, exp_id);
    }

    fn add_fun(&mut self, name: String, ty: Ty, bodies: Vec<ExpId>) -> FunId {
        let fun_id = FunId::new(self.sema.funs.len());
        let fun_def = FunDef {
            name,
            ty,
            bodies,
            symbols: vec![],
        };
        self.sema.funs.insert(fun_id, fun_def);
        self.current_fun_mut().symbols.push(SymbolKind::Fun(fun_id));
        fun_id
    }

    fn add_var(&mut self, var_def: VarDef) -> VarId {
        let var_id = VarId::new(self.sema.vars.len());

        self.sema.vars.insert(var_id, var_def);
        self.current_fun_mut().symbols.push(SymbolKind::Var(var_id));

        var_id
    }

    fn add_global(&mut self, name: String, ty: Ty) -> VarId {
        let index = self.sema.fun_local_count(GLOBAL_FUN_ID);
        let kind = VarKind::Global { index };
        self.add_var(VarDef { name, ty, kind })
    }

    fn add_local(&mut self, name: String, ty: Ty) -> VarId {
        let index = self.sema.fun_local_count(self.current_fun_id);
        let kind = VarKind::Local { index };
        self.add_var(VarDef { name, ty, kind })
    }

    fn add_arg(&mut self, name: String, ty: Ty, index: usize) -> VarId {
        let kind = VarKind::Arg { index };
        self.add_var(VarDef { name, ty, kind })
    }

    fn set_ty(&mut self, exp_id: ExpId, l_ty: &Ty, r_ty: &Ty) {
        self.sema.unify_ty(exp_id, &Ty::Var(exp_id), l_ty);
        self.sema.unify_ty(exp_id, l_ty, r_ty);
    }

    fn on_pat(&mut self, exp_id: ExpId, ty: Ty, arg_index: Option<usize>) {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            ExpKind::Err(message) => {
                self.add_err(message.to_string(), exp_id);
            }
            ExpKind::Ident(name) => {
                let var_id = if let Some(index) = arg_index {
                    self.add_arg(name.to_string(), ty.to_owned(), index)
                } else if self.current_fun_id == GLOBAL_FUN_ID {
                    self.add_global(name.to_string(), ty.to_owned())
                } else {
                    self.add_local(name.to_string(), ty.to_owned())
                };
                self.sema
                    .exp_symbols
                    .insert(exp_id, SymbolKind::Var(var_id));

                self.set_ty(exp_id, &ty, &ty);
            }
            _ => panic!("pattern must be an ident"),
        }
    }

    fn on_index_ref(&mut self, exp_id: ExpId, ty: Ty, indexee: ExpId, arg: ExpId) {
        self.on_val(indexee, Ty::Ptr);
        self.on_val(arg, Ty::Int);
        self.set_ty(exp_id, &ty, &Ty::Byte);
    }

    fn on_ref(&mut self, exp_id: ExpId, ty: Ty) {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            ExpKind::Err(message) => {
                self.add_err(message.to_string(), exp_id);
            }
            ExpKind::Ident(name) => {
                let symbol_kind = match self.lookup_symbol(name) {
                    Some(symbol) => symbol.kind(),
                    None => {
                        self.add_err("Undefined name".to_string(), exp_id);
                        return;
                    }
                };

                self.sema.exp_symbols.insert(exp_id, symbol_kind);

                match symbol_kind {
                    SymbolKind::Prim(..) => unimplemented!(),
                    SymbolKind::Var(..) => {
                        self.set_ty(exp_id, &ty, &ty);
                    }
                    SymbolKind::Fun(..) => unimplemented!(),
                }
            }
            &ExpKind::Index { indexee, arg } => {
                self.on_index_ref(exp_id, ty, indexee, arg);
            }
            _ => panic!("reference must be identifier"),
        }
    }

    fn on_ident(&mut self, exp_id: ExpId, ty: Ty, name: &str) {
        let (symbol_kind, symbol_ty) = match self.lookup_symbol(&name) {
            Some(symbol) => (symbol.kind(), symbol.get_ty()),
            None => {
                self.add_err("Undefined name".to_string(), exp_id);
                return;
            }
        };

        self.sema.exp_symbols.insert(exp_id, symbol_kind);

        self.set_ty(exp_id, &ty, &symbol_ty);

        match symbol_kind {
            SymbolKind::Prim(..) | SymbolKind::Fun(..) => {}
            SymbolKind::Var(..) => {
                self.sema.exp_vals.insert(exp_id);
            }
        }
    }

    fn on_call(&mut self, exp_id: ExpId, ty: Ty, callee: ExpId, args: &[ExpId]) {
        self.set_ty(exp_id, &ty, &Ty::Var(exp_id));
        let callee_ty = Ty::make_fun(args.iter().cloned().map(Ty::Var), ty);

        self.on_val(callee, callee_ty);
        for &arg in args {
            self.on_val(arg, Ty::Var(arg));
        }
    }

    fn on_index(&mut self, exp_id: ExpId, ty: Ty, indexee: ExpId, arg: ExpId) {
        self.on_index_ref(exp_id, ty, indexee, arg);
        self.sema.exp_vals.insert(exp_id);
    }

    fn on_bin(&mut self, exp_id: ExpId, ty: Ty, op: Op, exp_l: ExpId, exp_r: ExpId) {
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

    fn on_val(&mut self, exp_id: ExpId, ty: Ty) {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            ExpKind::Err(message) => {
                self.add_err(message.to_string(), exp_id);
            }
            &ExpKind::Int(_) => {
                self.set_ty(exp_id, &ty, &Ty::Int);
            }
            ExpKind::Str(_) => {
                self.set_ty(exp_id, &ty, &Ty::make_str());
            }
            ExpKind::Ident(name) => self.on_ident(exp_id, ty, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, ty, *callee, &args),
            ExpKind::Index { indexee, arg } => self.on_index(exp_id, ty, *indexee, *arg),
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, ty, op, l, r),
            ExpKind::Fun { .. } => {
                self.add_err(
                    "`fun` expressions must appear in the form of `let name = fun ..;`".to_string(),
                    exp_id,
                );
            }
            &ExpKind::If { cond, body, alt } => {
                self.on_val(cond, Ty::Int);
                self.on_val(body, Ty::Var(exp_id));
                self.on_val(alt, Ty::Var(exp_id));
                self.set_ty(exp_id, &Ty::Var(exp_id), &ty);
            }
            &ExpKind::While { cond, body } => {
                self.on_val(cond, Ty::Int);
                self.on_val(body, Ty::Unit);
                self.set_ty(exp_id, &ty, &Ty::Unit);
            }
            &ExpKind::Let { pat, init } => {
                let syntax = self.share_syntax();
                match (syntax.exp_kind(pat), syntax.exp_kind(init)) {
                    (ExpKind::Ident(fun_name), ExpKind::Fun { pats, body }) => {
                        self.set_ty(exp_id, &ty, &Ty::Unit);

                        let fun_ty =
                            Ty::make_fun(pats.iter().cloned().map(Ty::Var), Ty::Var(*body));
                        self.on_pat(pat, fun_ty.to_owned(), None);

                        let outer_fun_id = self.current_fun_id;
                        let fun_id = self.add_fun(fun_name.to_string(), fun_ty, vec![*body]);
                        self.current_fun_id = fun_id;

                        for (i, pat) in pats.iter().cloned().enumerate() {
                            self.on_pat(pat, Ty::Var(pat), Some(i));
                        }
                        self.on_val(*body, Ty::Var(*body));

                        self.current_fun_id = outer_fun_id;
                    }
                    _ => {
                        self.on_pat(pat, Ty::Var(pat), None);
                        self.on_val(init, Ty::Var(pat));
                        self.set_ty(exp_id, &ty, &Ty::Unit);
                    }
                }
            }
            ExpKind::Semi(exps) => {
                self.on_vals(exps);

                let last_ty = exps.last().cloned().map(Ty::Var).unwrap_or(Ty::Unit);
                self.set_ty(exp_id, &ty, &last_ty);
            }
        }
    }

    fn on_vals(&mut self, exp_ids: &[ExpId]) {
        for &exp_id in exp_ids {
            self.on_val(exp_id, Ty::Var(exp_id));
        }
    }

    fn sema(&mut self) {
        // Link all roots to a function.
        let roots = self.sema.syntax.roots.to_owned();
        let last_exp_id = *roots.last().expect("At least one document");
        let main_fun_ty = Ty::make_fun(iter::empty(), Ty::Var(last_exp_id));
        let fun_id = self.add_fun("main".to_string(), main_fun_ty, roots.to_owned());
        assert_eq!(fun_id, GLOBAL_FUN_ID);
        self.current_fun_id = GLOBAL_FUN_ID;

        for &(_, prim) in PRIMS {
            self.current_fun_mut().symbols.push(SymbolKind::Prim(prim));
        }

        self.on_vals(&roots);
    }
}

impl ShareSyntax for SemanticAnalyzer {
    fn share_syntax(&self) -> Rc<Syntax> {
        self.sema.syntax.clone()
    }
}

impl Sema {
    fn add_err(&mut self, message: String, exp_id: ExpId) {
        self.add_err_msg(message, exp_id);
    }

    pub fn get_symbol_ref(&self, symbol_kind: SymbolKind) -> SymbolRef<'_> {
        match symbol_kind {
            SymbolKind::Prim(prim) => SymbolRef::Prim(prim),
            SymbolKind::Var(var_id) => SymbolRef::Var(var_id, &self.vars[&var_id]),
            SymbolKind::Fun(fun_id) => SymbolRef::Fun(fun_id, &self.funs[&fun_id]),
        }
    }

    fn fun_symbols(&self, fun_id: FunId) -> Vec<SymbolRef<'_>> {
        self.funs[&fun_id]
            .symbols
            .iter()
            .map(|&symbol_kind| self.get_symbol_ref(symbol_kind))
            .collect::<Vec<_>>()
    }

    pub fn fun_local_count(&self, fun_id: FunId) -> usize {
        self.fun_symbols(fun_id)
            .into_iter()
            .filter(|symbol_ref| symbol_ref.kind().is_var())
            .count()
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

pub(crate) fn sema(syntax: Rc<Syntax>) -> Sema {
    let mut analyzer = SemanticAnalyzer {
        sema: Sema {
            syntax: Rc::clone(&syntax),
            exp_symbols: BTreeMap::new(),
            exp_vals: BTreeSet::new(),
            exp_tys: BTreeMap::new(),
            vars: BTreeMap::new(),
            funs: BTreeMap::new(),
            msgs: BTreeMap::new(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    analyzer.sema();
    analyzer.sema
}
