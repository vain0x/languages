use super::*;
use crate::syntax::*;
use std::collections::{BTreeMap, BTreeSet};
use std::mem;
use std::rc::Rc;

pub(crate) struct SemanticAnalyzer {
    sema: Sema,
    current_fun_id: FunId,
    current_loop_id: Option<LoopId>,
}

impl SemanticAnalyzer {
    fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.sema.syntax.exps[&exp_id]
    }

    fn current_fun(&self) -> &FunDef {
        self.sema.funs.get(&self.current_fun_id).unwrap()
    }

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

    fn add_err(&mut self, kind: MsgKind, exp_id: ExpId) {
        self.sema.add_err_msg(kind, exp_id);
    }

    fn add_fun(&mut self, _: ExpId, name: String, result_ty: Ty, bodies: Vec<ExpId>) -> FunId {
        let fun_def = FunDef {
            name,
            result_ty,
            bodies,
            symbols: vec![],
        };

        let fun_id = FunId::new(self.sema.funs.len());
        self.sema.funs.insert(fun_id, fun_def);

        fun_id
    }

    /// Define a variable that is defined with `let rec` before analysis.
    fn add_var_rec(&mut self, exp_id: ExpId, name: String) -> VarId {
        let ty = self.fresh_meta_ty(exp_id);

        let var_def = VarDef {
            name: name.to_string(),
            ty_scheme: Err(ty),
            kind: VarKind::Rec(exp_id),
            def_exp_id: exp_id,
        };

        let var_id = VarId::new(self.sema.vars.len());
        self.sema.vars.insert(var_id, var_def);
        self.current_fun_mut().symbols.push(SymbolKind::Var(var_id));

        var_id
    }

    fn add_var(&mut self, var_def: VarDef) -> VarId {
        let def_exp_id = var_def.def_exp_id;

        // If the variable is defined with `let rec`,
        // it already has var id.
        let var_id = match self.sema.exp_symbols.get(&def_exp_id) {
            Some(&SymbolKind::Var(var_id)) => var_id,
            _ => {
                let var_id = VarId::new(self.sema.vars.len());
                self.current_fun_mut().symbols.push(SymbolKind::Var(var_id));
                var_id
            }
        };

        let old_var_def = self.sema.vars.insert(var_id, var_def);

        // Merge old definition.
        if let Some(var_def) = old_var_def {
            let l_ty = var_def.ty_scheme.unwrap_err();
            let r_ty = (self.sema.vars.get_mut(&var_id).unwrap().clone())
                .ty_scheme
                .unwrap_err();
            self.unify_ty(def_exp_id, l_ty, r_ty);
        }

        var_id
    }

    fn add_global(&mut self, name: String, ty: Ty, def_exp_id: ExpId) -> VarId {
        let index = self.sema.fun_local_count(GLOBAL_FUN_ID);
        let kind = VarKind::Global { index };
        self.add_var(VarDef {
            name,
            ty_scheme: Err(ty),
            kind,
            def_exp_id,
        })
    }

    fn add_local(&mut self, name: String, ty: Ty, def_exp_id: ExpId) -> VarId {
        let index = self.sema.fun_local_count(self.current_fun_id);
        let kind = VarKind::Local { index };
        self.add_var(VarDef {
            name,
            ty_scheme: Err(ty),
            kind,
            def_exp_id,
        })
    }

    fn add_arg(&mut self, name: String, ty: Ty, index: usize, def_exp_id: ExpId) -> VarId {
        let kind = VarKind::Arg { index };
        self.add_var(VarDef {
            name,
            ty_scheme: Err(ty),
            kind,
            def_exp_id,
        })
    }

    fn add_var_fun(&mut self, name: String, ty: Ty, fun_id: FunId, def_exp_id: ExpId) -> VarId {
        self.add_var(VarDef {
            name,
            ty_scheme: Err(ty),
            kind: VarKind::Fun(fun_id),
            def_exp_id,
        })
    }

    // Generalize type of variable to type scheme.
    fn generalize_var(&mut self, var_id: VarId) {
        let ty = {
            let var_def = &self.sema.vars[&var_id];
            var_def.ty_scheme.clone().unwrap_err()
        };

        // Main type.
        let ty = self.sema.subst_ty(ty);

        // Generalize if function. Otherwise, keep meta variables open.
        let var_def = self.sema.vars.get_mut(&var_id).unwrap();

        let ty_scheme = if let VarKind::Fun(_) = var_def.kind {
            TyScheme::generalize(ty)
        } else {
            TyScheme::from(ty)
        };

        var_def.ty_scheme = Ok(ty_scheme);
    }

    fn add_loop(&mut self, exp_id: ExpId, body: ExpId) -> LoopId {
        let loop_id = LoopId::new(self.sema.loops.len());
        self.sema.loops.insert(loop_id, LoopDef { body });

        self.sema.exp_loops.insert(exp_id, loop_id);

        loop_id
    }

    fn set_symbol(&mut self, exp_id: ExpId, symbol_kind: SymbolKind) {
        self.sema.exp_symbols.insert(exp_id, symbol_kind);
    }

    fn set_ty(&mut self, exp_id: ExpId, ty: Ty) {
        self.sema.exp_tys.insert(exp_id, ty);
    }

    fn unify_ty(&mut self, exp_id: ExpId, l_ty: Ty, r_ty: Ty) {
        self.sema.unify_ty(exp_id, l_ty, r_ty);
    }

    fn fresh_meta_ty(&mut self, exp_id: ExpId) -> Ty {
        self.sema.fresh_meta_ty(exp_id)
    }

    fn instantiate_ty_scheme(&mut self, exp_id: ExpId, ty_scheme: TyScheme) -> Ty {
        ty_scheme.instantiate(|| self.fresh_meta_ty(exp_id))
    }

    fn on_ty(&mut self, exp_id: ExpId) -> Ty {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        self.set_ty(exp_id, Ty::unit()); // provisional
        match &exp.kind {
            ExpKind::Err(err) => {
                self.add_err(MsgKind::SyntaxError(err.clone()), exp_id);
                self.fresh_meta_ty(exp_id)
            }
            ExpKind::Ident(ident) => match ident.as_str() {
                "unit" => Ty::unit(),
                "byte" => Ty::byte(),
                "int" => Ty::int(),
                "_" => self.fresh_meta_ty(exp_id),
                _ => {
                    self.add_err(
                        MsgKind::Unimplemented("Can't use type variables".to_string()),
                        exp_id,
                    );
                    self.fresh_meta_ty(exp_id)
                }
            },
            _ => {
                self.add_err(
                    MsgKind::Unimplemented("Unsupported type expression".to_string()),
                    exp_id,
                );
                self.fresh_meta_ty(exp_id)
            }
        }
    }

    fn on_pat(&mut self, exp_id: ExpId, ty: Ty, arg_index: Option<usize>) -> Option<VarId> {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        self.set_ty(exp_id, ty.clone());
        match &exp.kind {
            ExpKind::Err(err) => {
                self.add_err(MsgKind::SyntaxError(err.clone()), exp_id);
                None
            }
            ExpKind::Ident(name) => {
                let var_id = if let Some(index) = arg_index {
                    self.add_arg(name.to_string(), ty, index, exp_id)
                } else if self.current_fun_id == GLOBAL_FUN_ID {
                    self.add_global(name.to_string(), ty, exp_id)
                } else {
                    self.add_local(name.to_string(), ty, exp_id)
                };
                self.set_symbol(exp_id, SymbolKind::Var(var_id));
                Some(var_id)
            }
            _ => panic!("pattern must be an ident"),
        }
    }

    fn on_range_or_int_val(&mut self, arg: ExpId) -> bool {
        match &self.exp(arg).kind {
            &ExpKind::Bin {
                op: Op::Range,
                l,
                r,
                ..
            } => {
                self.on_val(l, Ty::int());
                self.on_val(r, Ty::int());
                self.set_ty(arg, Ty::unit()); // No range type for now.
                self.sema.exp_ranges.insert(arg, (l, r));
                true
            }
            _ => {
                self.on_val(arg, Ty::int());
                false
            }
        }
    }

    fn on_index_ref(&mut self, exp_id: ExpId, ty: Ty, indexee: ExpId, arg: ExpId) {
        let inner_ty = self.fresh_meta_ty(exp_id);

        self.on_val(indexee, Ty::ptr(inner_ty.clone()));
        self.on_val(arg, Ty::int());

        self.unify_ty(exp_id, ty, inner_ty);
    }

    fn on_ref(&mut self, exp_id: ExpId, ty: Ty) {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        self.set_ty(exp_id, ty.clone());
        match &exp.kind {
            ExpKind::Err(err) => {
                self.add_err(MsgKind::SyntaxError(err.clone()), exp_id);
            }
            ExpKind::Ident(name) => {
                let (symbol_kind, symbol_ty) = match self.lookup_symbol(name) {
                    Some(symbol) => (symbol.kind(), symbol.ty_scheme()),
                    None => {
                        self.add_err(MsgKind::Undefined, exp_id);
                        return;
                    }
                };

                self.set_symbol(exp_id, symbol_kind);

                let symbol_ty = self.instantiate_ty_scheme(exp_id, symbol_ty);
                self.unify_ty(exp_id, ty, symbol_ty);
            }
            &ExpKind::Index { indexee, arg } => {
                self.on_index_ref(exp_id, ty, indexee, arg);
            }
            &ExpKind::Bin { op: Op::Anno, l, r } => {
                let anno_ty = self.on_ty(r);
                self.on_ref(l, anno_ty.clone());
                self.unify_ty(exp_id, ty, anno_ty);
            }
            _ => panic!("reference must be identifier"),
        }
    }

    fn on_ident(&mut self, exp_id: ExpId, ty: Ty, name: &str) {
        let (symbol_kind, symbol_ty) = match self.lookup_symbol(&name) {
            Some(symbol) => (symbol.kind(), symbol.ty_scheme()),
            None => {
                self.add_err(MsgKind::Undefined, exp_id);
                return;
            }
        };

        self.set_symbol(exp_id, symbol_kind);

        let symbol_ty = self.instantiate_ty_scheme(exp_id, symbol_ty);
        self.unify_ty(exp_id, ty, symbol_ty);

        match symbol_kind {
            SymbolKind::Prim(..) => {}
            SymbolKind::Var(..) => {
                self.sema.exp_vals.insert(exp_id);
            }
        }
    }

    fn on_call(&mut self, _: ExpId, ty: Ty, callee: ExpId, args: &[ExpId]) {
        let arg_tys = args
            .iter()
            .map(|&arg| self.sema.fresh_meta_ty(arg))
            .collect::<Vec<_>>();
        let callee_ty = Ty::make_fun(arg_tys.clone(), ty);

        self.on_val(callee, callee_ty);
        for (&arg, arg_ty) in args.into_iter().zip(arg_tys) {
            self.on_val(arg, arg_ty);
        }
    }

    fn on_index(&mut self, exp_id: ExpId, ty: Ty, indexee: ExpId, arg: ExpId) {
        let inner_ty = self.fresh_meta_ty(exp_id);

        self.on_val(indexee, Ty::ptr(inner_ty.clone()));
        let range = self.on_range_or_int_val(arg);

        let result_ty = if range { Ty::ptr(inner_ty) } else { inner_ty };
        self.unify_ty(exp_id, ty, result_ty);

        self.sema.exp_vals.insert(exp_id);
    }

    fn on_bin(&mut self, exp_id: ExpId, ty: Ty, op: Op, exp_l: ExpId, exp_r: ExpId) {
        match op {
            Op::Set | Op::SetAdd | Op::SetSub | Op::SetMul | Op::SetDiv | Op::SetMod => {
                let item_ty = self.fresh_meta_ty(exp_l);

                self.on_ref(exp_l, item_ty.clone());
                self.on_val(exp_r, item_ty.clone());
                self.unify_ty(exp_id, ty, Ty::unit());

                match self.sema.subst_ty(item_ty) {
                    Ty::Con(TyCon::Byte, _) | Ty::Con(TyCon::Int, _) | Ty::Con(TyCon::Slice, _) => {
                    }
                    ty => self.add_err(
                        MsgKind::Unimplemented(format!(
                            "You cannot assign non-byte/int/slice value for now {:?}",
                            ty
                        )),
                        exp_id,
                    ),
                }
            }
            Op::Range => {
                self.on_val(exp_l, Ty::int());
                self.on_val(exp_r, Ty::int());
                self.add_err(MsgKind::InvalidUseOfRange, exp_id)
            }
            Op::Anno => {
                let anno_ty = self.on_ty(exp_r);
                self.on_val(exp_l, anno_ty.clone());
                self.unify_ty(exp_id, ty, anno_ty);
            }
            Op::As => {
                let dest_ty = self.on_ty(exp_r);
                let l_ty = self.fresh_meta_ty(exp_l);
                self.on_val(exp_l, l_ty.clone());
                self.unify_ty(exp_id, ty, dest_ty.clone());

                if !l_ty.can_cast_to(&dest_ty) {
                    self.add_err(MsgKind::InvalidTypeCast(l_ty, dest_ty), exp_id);
                }
            }
            _ => {
                self.on_val(exp_l, Ty::int());
                self.on_val(exp_r, Ty::int());
                self.unify_ty(exp_id, ty, Ty::int());
            }
        }
    }

    fn on_val(&mut self, exp_id: ExpId, ty: Ty) {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        self.set_ty(exp_id, ty.clone());
        match &exp.kind {
            ExpKind::Err(err) => {
                self.add_err(MsgKind::SyntaxError(err.clone()), exp_id);
            }
            &ExpKind::Unit => {
                self.unify_ty(exp_id, ty, Ty::unit());
            }
            &ExpKind::Int(_) => {
                self.unify_ty(exp_id, ty, Ty::int());
            }
            &ExpKind::Byte(_) => {
                self.unify_ty(exp_id, ty, Ty::byte());
            }
            ExpKind::Str(_) => {
                self.unify_ty(exp_id, ty, Ty::make_str());
            }
            ExpKind::Ident(name) => self.on_ident(exp_id, ty, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, ty, *callee, &args),
            ExpKind::Index { indexee, arg } => self.on_index(exp_id, ty, *indexee, *arg),
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, ty, op, l, r),
            ExpKind::Fun { .. } => {
                self.add_err(
                    MsgKind::Unimplemented(
                        "Functions must appear in the form of `let name = || ..`".to_string(),
                    ),
                    exp_id,
                );
            }
            &ExpKind::Return(result) => {
                let result_ty = self.current_fun().result_ty();
                self.on_val(result, result_ty);
            }
            &ExpKind::If { cond, body, alt } => {
                self.on_val(cond, Ty::int());
                self.on_val(body, ty.clone());
                self.on_val(alt, ty);
            }
            &ExpKind::While { cond, body } => {
                let loop_id = self.add_loop(exp_id, body);
                let outer_loop_id = mem::replace(&mut self.current_loop_id, Some(loop_id));

                self.on_val(cond, Ty::int());
                self.on_val(body, Ty::unit());
                self.unify_ty(exp_id, ty, Ty::unit());

                self.current_loop_id = outer_loop_id;
            }
            &ExpKind::Break | &ExpKind::Continue => {
                let loop_id = match self.current_loop_id {
                    None => {
                        self.add_err(MsgKind::OutOfLoop, exp_id);
                        return;
                    }
                    Some(loop_id) => loop_id,
                };

                self.sema.exp_loops.insert(exp_id, loop_id);
            }
            &ExpKind::Let {
                pat, pat_ty, init, ..
            } => {
                let syntax = self.share_syntax();
                let pat_ty = if let Some(pat_ty) = pat_ty {
                    self.on_ty(pat_ty)
                } else {
                    self.fresh_meta_ty(pat)
                };
                match (syntax.exp_kind(pat), syntax.exp_kind(init)) {
                    (ExpKind::Ident(fun_name), ExpKind::Fun { pats, body }) => {
                        self.unify_ty(exp_id, ty, Ty::unit());
                        self.sema.exp_decls.insert(exp_id);

                        let arg_tys = pats
                            .iter()
                            .map(|&pat| self.fresh_meta_ty(pat))
                            .collect::<Vec<_>>();
                        let result_ty = self.fresh_meta_ty(*body);
                        let fun_ty = Ty::make_fun(arg_tys.clone(), result_ty.clone());
                        self.unify_ty(exp_id, fun_ty.clone(), pat_ty);

                        let fun_id =
                            self.add_fun(pat, fun_name.to_string(), result_ty.clone(), vec![*body]);

                        let outer_fun_id = mem::replace(&mut self.current_fun_id, fun_id);
                        let outer_loop_id = mem::replace(&mut self.current_loop_id, None);

                        let mut var_ids = vec![];
                        for (i, (pat, arg_ty)) in pats.iter().cloned().zip(arg_tys).enumerate() {
                            let var_id = self.on_pat(pat, arg_ty, Some(i));

                            var_ids.extend(var_id.into_iter());
                        }
                        self.on_val(*body, result_ty);

                        self.current_fun_id = outer_fun_id;
                        self.current_loop_id = outer_loop_id;

                        let fun_var_id =
                            self.add_var_fun(fun_name.to_string(), fun_ty.clone(), fun_id, pat);
                        var_ids.push(fun_var_id);

                        self.set_ty(pat, fun_ty.clone());
                        self.set_ty(init, fun_ty);

                        // Generalize types introduced in the definition.
                        for var_id in var_ids {
                            self.generalize_var(var_id);
                        }
                    }
                    _ => {
                        self.on_val(init, pat_ty.clone());
                        let var_id = self.on_pat(pat, pat_ty, None);
                        self.unify_ty(exp_id, ty, Ty::unit());

                        if let Some(var_id) = var_id {
                            self.generalize_var(var_id);
                        }
                    }
                }
            }
            ExpKind::Semi(exps) => {
                self.on_vals(exps, ty);
            }
        }
    }

    fn look_ahead_let_rec(&mut self, exp_id: ExpId) {
        let pat = match &self.exp(exp_id).kind {
            &ExpKind::Let { pat, rec: true, .. } => pat,
            _ => return,
        };

        let name = match &self.exp(pat).kind {
            ExpKind::Ident(name) => name.to_string(),
            _ => return,
        };

        let var_id = self.add_var_rec(pat, name);
        self.current_fun_mut().symbols.push(SymbolKind::Var(var_id));
        self.set_symbol(pat, SymbolKind::Var(var_id));
    }

    fn on_vals(&mut self, exp_ids: &[ExpId], last_ty: Ty) {
        for &exp_id in exp_ids {
            self.look_ahead_let_rec(exp_id);
        }

        for i in 0..exp_ids.len() {
            let ty = {
                let is_last = i + 1 == exp_ids.len();
                if is_last {
                    last_ty.clone()
                } else {
                    self.fresh_meta_ty(exp_ids[i])
                }
            };

            self.on_val(exp_ids[i], ty);
        }
    }

    fn verify(&mut self) {
        // Recursive variables must be set actual kind.
        let var_ids = self.sema.vars.keys().cloned().collect::<Vec<_>>();
        for &var_id in &var_ids {
            if let VarKind::Rec(exp_id) = self.sema.vars[&var_id].kind {
                self.add_err(
                    MsgKind::Unexpected("Unresolved let-rec variable".to_string()),
                    exp_id,
                );
            }
        }

        // All variables must be generalized.
        for &var_id in &var_ids {
            if self.sema.vars[&var_id].ty_scheme.is_err() {
                self.add_err(
                    MsgKind::Unexpected("Variable not generalized".to_string()),
                    self.sema.vars[&var_id].def_exp_id,
                );
            }
        }

        // All expressions must have a type.
        let exp_ids = self.share_syntax().exps.keys().cloned().collect::<Vec<_>>();
        for &exp_id in &exp_ids {
            match self.sema.exp_tys.get(&exp_id) {
                None => self.add_err(
                    MsgKind::Unexpected(format!(
                        "Expression must have type ({})",
                        self.sema.exp_text(exp_id)
                    )),
                    exp_id,
                ),
                Some(_) => {}
            }
        }
    }

    fn analyze(&mut self) {
        // Merge all top-level expressions into single function.
        let roots = self.sema.syntax.module_root_exps().collect::<Vec<_>>();
        let last_exp_id = *roots.last().unwrap();
        let main_result_ty = self.fresh_meta_ty(last_exp_id);
        let fun_id = self.add_fun(
            last_exp_id,
            "main".to_string(),
            main_result_ty.clone(),
            roots.to_owned(),
        );
        assert_eq!(fun_id, GLOBAL_FUN_ID);
        self.current_fun_id = GLOBAL_FUN_ID;

        for &(_, prim) in PRIMS {
            self.current_fun_mut().symbols.push(SymbolKind::Prim(prim));
        }

        self.on_vals(&roots, main_result_ty);
        // FIXME: don't allow function type

        self.verify();
    }
}

impl ShareSyntax for SemanticAnalyzer {
    fn share_syntax(&self) -> Rc<Syntax> {
        self.sema.syntax.clone()
    }
}

impl Sema {
    fn add_err(&mut self, kind: MsgKind, exp_id: ExpId) {
        self.add_err_msg(kind, exp_id);
    }

    pub fn fun_local_count(&self, fun_id: FunId) -> usize {
        self.fun_symbols(fun_id)
            .into_iter()
            .filter(|symbol_ref| symbol_ref.kind().is_var())
            .count()
    }

    pub fn get_ty(&self, exp_id: ExpId) -> Ty {
        let ty = self.exp_tys.get(&exp_id).cloned().unwrap();
        self.subst_ty(ty)
    }

    fn fresh_meta_ty(&mut self, exp_id: ExpId) -> Ty {
        let ty_id = TyId::new(self.tys.len());
        self.tys.insert(ty_id, TyDef { ty: None, exp_id });
        Ty::Meta(ty_id)
    }

    fn do_bind_ty(&mut self, ty_id: TyId, ty: Ty) {
        let ty_def = self.tys.get_mut(&ty_id).unwrap();
        match ty_def.ty {
            Some(_) => panic!("Cannot re-bind meta ty"),
            None => {
                ty_def.ty = Some(ty);
            }
        }
    }

    fn subst_ty(&self, ty: Ty) -> Ty {
        ty.replace_with(&mut |ty_id| self.tys[&ty_id].ty.clone().unwrap_or(Ty::Meta(ty_id)))
    }

    fn unify_ty(&mut self, exp_id: ExpId, l_ty: Ty, r_ty: Ty) {
        let l_ty = self.subst_ty(l_ty.clone());
        let r_ty = self.subst_ty(r_ty.clone());
        match (l_ty, r_ty) {
            (Ty::Meta(l), Ty::Meta(r)) if l == r => {}
            (Ty::Meta(l), r_ty) => self.do_bind_ty(l, r_ty),
            (l_ty, Ty::Meta(r)) => self.do_bind_ty(r, l_ty),
            (Ty::Err, _) | (_, Ty::Err) => {}
            (Ty::Con(l_ty_con, ref l_tys), Ty::Con(r_ty_con, ref r_tys))
                if l_ty_con == r_ty_con =>
            {
                self.unify_tys(exp_id, l_ty_con, l_tys.clone(), r_ty_con, r_tys.clone())
            }
            (l_ty, r_ty) => {
                eprintln!("Type Error left={:?} right={:?}", l_ty, r_ty);
                self.add_err(MsgKind::TypeMismatch(l_ty, r_ty), exp_id)
            }
        }
    }

    fn unify_tys(
        &mut self,
        exp_id: ExpId,
        l_ty_con: TyCon,
        l_tys: Vec<Ty>,
        r_ty_con: TyCon,
        r_tys: Vec<Ty>,
    ) {
        if l_tys.len() != r_tys.len() {
            let l_ty = Ty::Con(l_ty_con, l_tys);
            let r_ty = Ty::Con(r_ty_con, r_tys);
            self.add_err(MsgKind::TypeMismatch(l_ty, r_ty), exp_id);
            return;
        }

        for (l_ty, r_ty) in l_tys.into_iter().zip(r_tys.into_iter()) {
            self.unify_ty(exp_id, l_ty, r_ty);
        }
    }

    fn calculate_parents(&mut self) {
        for &parent in Rc::clone(&self.syntax).exps.keys() {
            let children = self.exp(parent).kind.children();
            for child_id in children {
                self.exp_parent.insert(child_id, parent);
            }
        }
    }
}

pub(crate) fn sema(syntax: Rc<Syntax>) -> Sema {
    let mut analyzer = SemanticAnalyzer {
        sema: Sema {
            syntax: Rc::clone(&syntax),
            exp_symbols: BTreeMap::new(),
            exp_loops: BTreeMap::new(),
            exp_vals: BTreeSet::new(),
            exp_ranges: BTreeMap::new(),
            exp_decls: BTreeSet::new(),
            exp_tys: BTreeMap::new(),
            exp_parent: BTreeMap::new(),
            tys: BTreeMap::new(),
            vars: BTreeMap::new(),
            funs: BTreeMap::new(),
            loops: BTreeMap::new(),
            msgs: BTreeMap::new(),
        },
        current_fun_id: GLOBAL_FUN_ID,
        current_loop_id: None,
    };
    analyzer.analyze();
    analyzer.sema.calculate_parents();
    analyzer.sema
}

pub(crate) fn analyze_doc(doc_id: DocId, src: Rc<String>) -> Sema {
    static PRELUDE: &str = include_str!("../../stdlib/prelude.picomet");

    let mut syntax = Syntax::default();
    let prelude_doc = Rc::new(Doc::new(
        DocId::new(std::usize::MAX),
        "prelude".to_string(),
        Rc::new(PRELUDE.to_string()),
    ));
    syntax.add_module(prelude_doc);

    let doc = Rc::new(Doc::new(doc_id, "main".to_string(), src));
    syntax.add_module(doc);

    sema(Rc::new(syntax))
}

pub(crate) fn analyze_str(src: &str) -> Sema {
    analyze_doc(DocId::new(0), Rc::new(src.to_string()))
}
