use super::*;

macro_rules! decompose {
    ($items:expr, [$($item:ident),*]) => {
        let mut iter = $items.into_iter();
        $(let $item = iter.next().expect(concat!("decompose! ", stringify!($item)));)*
    };
}

enum StmtResult {
    None,
    Keep,
    Set(VarId),
}

struct ReducePir {
    vars: BTreeMap<VarId, PirVarDef>,
    funs: BTreeMap<FunId, PirFunDef>,
}

fn make_pir(kind: PirKind, args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
    Pir {
        kind,
        args,
        ty,
        exp_id,
    }
}

fn make_semi(args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
    make_pir(PirKind::Semi, args, ty, exp_id)
}

impl ReducePir {
    /// Add declaration of fresh temporary variable.
    /// Return (var_id, var_pir) where var_pir refers to the value.
    fn add_temporary_begin(&mut self, ty: Ty, exp_id: ExpId) -> (VarId, Pir) {
        let var_id = VarId::new(self.vars.len());
        self.vars.insert(
            var_id,
            PirVarDef {
                is_temporary: true,
                exp_id,
            },
        );
        let var_pir = make_pir(PirKind::Var { var_id }, vec![], ty, exp_id).into_deref();
        (var_id, var_pir)
    }

    /// Add initialization of the temporary variable.
    fn add_temporary_end(&mut self, var_id: VarId, init: Pir, semi: &mut Vec<Pir>) {
        let (ty, exp_id) = (init.ty(), init.exp_id);
        let var_pir = make_pir(PirKind::Var { var_id }, vec![], ty, exp_id);
        semi.push(make_pir(
            PirKind::Op {
                op: Op::Set,
                size: 0,
            },
            vec![var_pir, init],
            Ty::unit(),
            exp_id,
        ));
    }

    fn add_temporary(&mut self, init: Pir, semi: &mut Vec<Pir>) -> Pir {
        let (ty, exp_id) = (init.ty(), init.exp_id);
        let (var_id, var_pir) = self.add_temporary_begin(ty, exp_id);
        self.add_temporary_end(var_id, init, semi);
        var_pir
    }

    /// Multiple an int by size of type.
    fn scale_up(&mut self, arg: &mut Pir, slice_ty: Ty) {
        let inner_ty = slice_ty.as_slice_inner().expect("Expected slice type");
        let size = inner_ty.size_of().expect("Expected sized type");
        let scale = arg.clone().mul_int(size as i64);
        *arg = scale;
    }

    fn scale_down(&mut self, arg: &mut Pir, slice_ty: Ty) {
        let inner_ty = slice_ty.as_slice_inner().expect("Expected slice type");
        let size = inner_ty.size_of().expect("Expected sized type");
        *arg = arg.clone().div_int(size as i64);
    }

    fn scale_ptr_indices(&mut self, kind: PirKind, args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
        let mut args = args
            .into_iter()
            .map(|arg| self.scale_ptr_indices(arg.kind, arg.args, arg.ty, arg.exp_id))
            .collect::<Vec<_>>();
        match kind {
            PirKind::CallPrim(Prim::SliceLen) => {
                let slice_ty = args[0].ty();
                let mut pir = make_pir(kind, args, ty, exp_id);
                self.scale_down(&mut pir, slice_ty);
                pir
            }
            PirKind::CallPrim(Prim::MemAlloc) => {
                self.scale_up(&mut args[0], ty.clone());
                make_pir(kind, args, ty, exp_id)
            }
            PirKind::IndexPoint => {
                let slice_ty = args[0].ty();
                self.scale_up(&mut args[1], slice_ty);
                make_pir(kind, args, ty, exp_id)
            }
            PirKind::IndexSlice => {
                let slice_ty = args[0].ty();
                self.scale_up(&mut args[1], slice_ty.clone());
                self.scale_up(&mut args[2], slice_ty);
                make_pir(kind, args, ty, exp_id)
            }
            _ => make_pir(kind, args, ty, exp_id),
        }
    }

    fn reduce_prim(&mut self, kind: PirKind, args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
        let args = args
            .into_iter()
            .map(|arg| self.reduce_prim(arg.kind, arg.args, arg.ty, arg.exp_id))
            .collect::<Vec<_>>();
        match kind {
            PirKind::CallPrim(Prim::SliceLen) => {
                // slice_len(xs) = slice_end(xs) - slice_begin(xs)
                decompose!(args, [xs]);
                let mut semi = vec![];
                let xs = self.add_temporary(xs, &mut semi);
                let end = xs.clone().slice_end();
                let begin = xs.clone().slice_begin();
                semi.push(end.sub(begin));
                make_semi(semi, ty, exp_id)
            }
            PirKind::IndexPoint => {
                // xs[i] = slice_begin(xs) + i
                decompose!(args, [xs, i]);
                let inner_ty = xs.ty().as_slice_inner().expect("indexee is slice");
                xs.slice_begin().add(i).with_ty(inner_ty)
            }
            PirKind::IndexSlice => {
                // xs[l..r] = slice_new(slice_begin(xs) + l, slice_begin(xs) + r)
                decompose!(args, [xs, l, r]);
                let mut semi = vec![];
                let xs = self.add_temporary(xs, &mut semi);
                let xl = xs.clone().slice_begin().add(l);
                let xr = xs.clone().slice_begin().add(r);
                semi.push(xl.slice_new(xr));
                make_semi(semi, ty, exp_id)
            }
            PirKind::Op { op: Op::LogOr, .. } => {
                // l || r ---> if l { true } else { r }
                decompose!(args, [l, r]);
                let true_pir = Pir::int_true(exp_id);
                make_pir(PirKind::If, vec![l, true_pir, r], ty, exp_id)
            }
            PirKind::Op { op: Op::LogAnd, .. } => {
                // l && r ---> if l { r } else { false }
                decompose!(args, [l, r]);
                let false_pir = Pir::int_false(exp_id);
                make_pir(PirKind::If, vec![l, r, false_pir], ty, exp_id)
            }
            PirKind::While { loop_id } => {
                // while p { x } ---> loop { if p { x } else { break } }
                decompose!(args, [cond, body]);
                let break_pir = Pir::break_stmt(loop_id, ty.clone(), exp_id);
                let if_pir = make_pir(PirKind::If, vec![cond, body, break_pir], ty.clone(), exp_id);
                make_pir(PirKind::Loop { loop_id }, vec![if_pir], ty, exp_id)
            }
            _ => make_pir(kind, args, ty, exp_id),
        }
    }

    /// Canonicalize the expression.
    /// In the result tree,
    /// - only statements such as `if` contain other statements
    /// - expressions contain only val/var as children
    ///     except for right-hand side of `=` may contain call expressions etc.
    /// E.g. `return f(if p { x } else { y })` --->
    /// ``
    /// let _f, _if;
    /// if p { _if = x } else { _if = y };
    /// _f = f(_if);
    /// return _f;
    /// ``
    fn canonicalize_exp(&mut self, mut pir: Pir, semi: &mut Vec<Pir>) -> Pir {
        match &pir.kind {
            PirKind::Op { op, .. } if op.is_stmt() => {
                let exp_id = pir.exp_id;
                let args = std::mem::replace(&mut pir.args, vec![]);
                let args = self.canonicalize_many(args, semi);
                semi.push(Pir { args, ..pir });
                Pir::unit(exp_id)
            }
            PirKind::Val(..) | PirKind::Var { .. } | PirKind::Op { .. } | PirKind::Deref { .. } => {
                let args = std::mem::replace(&mut pir.args, vec![]);
                let args = self.canonicalize_many(args, semi);
                Pir { args, ..pir }
            }
            PirKind::CallPrim(..) | PirKind::CallFun(..) => {
                let args = std::mem::replace(&mut pir.args, vec![]);
                let args = self.canonicalize_many(args, semi);
                let pir = Pir { args, ..pir };
                self.add_temporary(pir, semi)
            }
            PirKind::If => {
                let (ty, exp_id) = (pir.ty(), pir.exp_id);
                let args = std::mem::replace(&mut pir.args, vec![]);
                decompose!(args, [cond, body, alt]);

                let (var_id, result) = self.add_temporary_begin(ty, exp_id);
                let cond = self.canonicalize_exp(cond, semi);
                let body = self.canonicalize_stmt(body, StmtResult::Set(var_id));
                let alt = self.canonicalize_stmt(alt, StmtResult::Set(var_id));
                let args = vec![cond, body, alt];
                semi.push(Pir { args, ..pir });

                result
            }
            PirKind::Loop { .. } => {
                let exp_id = pir.exp_id;
                let args = std::mem::replace(&mut pir.args, vec![]);
                decompose!(args, [body]);

                let body = self.canonicalize_stmt(body, StmtResult::None);
                semi.push(Pir {
                    args: vec![body],
                    ..pir
                });

                Pir::unit(exp_id)
            }
            PirKind::Break { .. } | PirKind::Continue { .. } | PirKind::Return => {
                let exp_id = pir.exp_id;
                let args = std::mem::replace(&mut pir.args, vec![]);
                let args = self.canonicalize_many(args, semi);
                semi.push(Pir { args, ..pir });
                Pir::unit(exp_id)
            }
            PirKind::Semi => {
                let (args, ty, exp_id) = (pir.args, pir.ty, pir.exp_id);

                let (var_id, result) = self.add_temporary_begin(ty, exp_id);
                let last = args
                    .into_iter()
                    .map(|arg| self.canonicalize_exp(arg, semi))
                    .last()
                    .expect("semi have any child");
                self.add_temporary_end(var_id, last, semi);
                result
            }
            PirKind::IndexPoint | PirKind::IndexSlice | PirKind::While { .. } => {
                panic!("{:?} should be removed", pir.kind)
            }
        }
    }

    fn canonicalize_many(&mut self, pirs: Vec<Pir>, semi: &mut Vec<Pir>) -> Vec<Pir> {
        pirs.into_iter()
            .map(|pir| self.canonicalize_exp(pir, semi))
            .collect()
    }

    fn canonicalize_stmt(&mut self, pir: Pir, result: StmtResult) -> Pir {
        let (ty, exp_id) = (pir.ty(), pir.exp_id);

        let mut semi = vec![];
        let init = self.canonicalize_exp(pir, &mut semi);

        match result {
            StmtResult::None => {
                // We can discard `init` because it doesn't cause side-effects.
            }
            StmtResult::Keep => {
                semi.push(init);
            }
            StmtResult::Set(var_id) => {
                self.add_temporary_end(var_id, init, &mut semi);
            }
        }

        make_pir(PirKind::Semi, semi, ty, exp_id)
    }

    fn calc_size(&mut self, mut kind: PirKind, args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
        match kind {
            PirKind::Op { op, .. } => {
                decompose!(&args, [x]);
                let size = x.ty().size_of().expect("sized");
                kind = PirKind::Op { op, size };
            }
            PirKind::Deref { .. } => {
                let size = ty.size_of().expect("sized");
                kind = PirKind::Deref { size };
            }
            _ => {}
        }
        let args = args
            .into_iter()
            .map(|arg| self.calc_size(arg.kind, arg.args, arg.ty, arg.exp_id))
            .collect();
        make_pir(kind, args, ty, exp_id)
    }

    fn reduce_pir(&mut self, pir: Pir) -> Pir {
        let pir = self.scale_ptr_indices(pir.kind, pir.args, pir.ty, pir.exp_id);
        let pir = self.reduce_prim(pir.kind, pir.args, pir.ty, pir.exp_id);
        let pir = self.canonicalize_stmt(pir, StmtResult::Keep);
        let pir = self.calc_size(pir.kind, pir.args, pir.ty, pir.exp_id);
        pir
    }

    fn add_temporaries_to_locals(&mut self, fun_id: FunId) {
        fn dfs(pir: &Pir, var_ids: &mut Vec<VarId>) {
            if let PirKind::Var { var_id } = pir.kind {
                var_ids.push(var_id)
            }

            for pir in pir.children() {
                dfs(pir, var_ids)
            }
        }

        let body = &self.funs[&fun_id].body;

        let mut var_ids = vec![];
        dfs(body, &mut var_ids);
        var_ids.retain(|var_id| self.vars[&var_id].is_temporary);
        var_ids.sort();
        var_ids.dedup();

        self.funs.get_mut(&fun_id).unwrap().locals.extend(var_ids);
    }

    fn run(&mut self) {
        for fun_id in self.funs.keys().cloned().collect::<Vec<_>>() {
            let body = self.funs[&fun_id].body.clone();
            let body = self.reduce_pir(body);
            self.funs.get_mut(&fun_id).unwrap().body = body;

            self.add_temporaries_to_locals(fun_id);
        }
    }
}

pub(crate) fn reduce(program: PirProgram) -> PirProgram {
    let (vars, funs) = (program.vars, program.funs);

    let mut reduce_pir = ReducePir { vars, funs };
    reduce_pir.run();

    let (vars, funs) = (reduce_pir.vars, reduce_pir.funs);

    PirProgram { vars, funs }
}
