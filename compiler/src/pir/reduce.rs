use super::*;

macro_rules! decompose {
    ($items:expr, [$($item:ident),*]) => {
        let mut iter = $items.into_iter();
        $(let $item = iter.next().unwrap();)*
    };
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
    fn add_temporary(&mut self, pir: Pir, semi: &mut Vec<Pir>) -> Pir {
        let var_id = VarId::new(self.vars.len());
        self.vars.insert(
            var_id,
            PirVarDef {
                ty: pir.ty.clone(),
                exp_id: pir.exp_id,
            },
        );

        semi.push(make_pir(
            PirKind::Op {
                op: Op::Set,
                size: 0,
            },
            vec![],
            Ty::unit(),
            pir.exp_id,
        ));

        make_pir(PirKind::Var { var_id }, vec![], pir.ty, pir.exp_id)
    }

    /// Multiple an int by size of type.
    fn scale_up(&mut self, arg: &mut Pir, slice_ty: Ty) {
        let inner_ty = slice_ty.as_slice_inner().expect("Expected slice type");
        let size = inner_ty.size_of().expect("Expected sized type");
        let scale = arg.clone().mul_int(size as i64);
        *arg = scale;
    }

    fn scale_ptr_indices(
        &mut self,
        kind: PirKind,
        mut args: Vec<Pir>,
        ty: Ty,
        exp_id: ExpId,
    ) -> Pir {
        match kind {
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
            _ => {
                let args = args
                    .into_iter()
                    .map(|arg| self.scale_ptr_indices(arg.kind, arg.args, arg.ty, arg.exp_id))
                    .collect();
                make_pir(kind, args, ty, exp_id)
            }
        }
    }

    fn reduce_prim(&mut self, kind: PirKind, args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
        match kind {
            PirKind::CallPrim(Prim::ByteToInt) | PirKind::CallPrim(Prim::IntToByte) => {
                decompose!(args, [arg]);
                arg
            }
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
                xs.slice_begin().add(i)
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
            _ => {
                let args = args
                    .into_iter()
                    .map(|arg| self.reduce_prim(arg.kind, arg.args, arg.ty, arg.exp_id))
                    .collect();
                make_pir(kind, args, ty, exp_id)
            }
        }
    }

    fn calc_size(&mut self, mut kind: PirKind, args: Vec<Pir>, ty: Ty, exp_id: ExpId) -> Pir {
        match kind {
            PirKind::Op { op, .. } => {
                decompose!(&args, [x]);
                let size = x.ty().size_of().expect("sized");
                kind = PirKind::Op { op, size };
            }
            PirKind::Deref { .. } => {
                decompose!(&args, [p]);
                let size = p.ty().size_of().expect("sized");
                kind = PirKind::Deref { size };
            }
            _ => {}
        }
        let args = args
            .into_iter()
            .map(|arg| self.reduce_prim(arg.kind, arg.args, arg.ty, arg.exp_id))
            .collect();
        make_pir(kind, args, ty, exp_id)
    }

    fn reduce_pir(&mut self, pir: Pir) -> Pir {
        let pir = self.scale_ptr_indices(pir.kind, pir.args, pir.ty, pir.exp_id);
        let pir = self.reduce_prim(pir.kind, pir.args, pir.ty, pir.exp_id);
        let pir = self.calc_size(pir.kind, pir.args, pir.ty, pir.exp_id);
        pir
    }

    fn run(&mut self) {
        for fun_id in self.funs.keys().cloned().collect::<Vec<_>>() {
            let body = self.funs[&fun_id].body.clone();
            let body = self.reduce_pir(body);
            self.funs.get_mut(&fun_id).unwrap().body = body;
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
