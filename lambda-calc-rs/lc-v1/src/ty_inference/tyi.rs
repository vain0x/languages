#![allow(unused)]

use super::*;

// 参考:
//    - [Efficient and Insightful Generalization](http://okmij.org/ftp/ML/generalization.html)
//    - Didier Rémy の論文: [eq-theory-on-types.pdf](http://gallium.inria.fr/~remy/ftp/eq-theory-on-types.pdf)
//    - [OCaml でも採用されているレベルベースの多相型型推論とは](https://rhysd.hatenablog.com/entry/2017/12/16/002048)

pub(crate) type Level = usize;

/// 型を持つシンボルの ID. (変数など)
pub(crate) type TyiSymbol = usize;

#[derive(Copy, Clone)]
pub(crate) struct TyiContext<'a> {
    last_id: &'a Cell<usize>,
    map: &'a RefCell<HashMap<TyiSymbol, TyScheme<'a>>>,
    level: &'a Cell<Level>,
    bump: &'a Bump,
}

impl<'a> TyiContext<'a> {
    fn fresh_id(self) -> usize {
        let id = self.last_id.get() + 1;
        self.last_id.set(id);
        id
    }

    fn level_up(self) {
        self.level.set(self.level.get() + 1);
    }

    fn level_down(self) {
        self.level.set(self.level.get() - 1);
    }

    fn add_symbol(self, hint: &'a str, symbol: TyiSymbol, ty_opt: Option<TyScheme<'a>>) {
        let ty = match ty_opt {
            Some(it) => it,
            None => TyScheme::Mono(Tyi::Var(VarTyi::new(hint, self))),
        };
        self.map.borrow_mut().insert(symbol, ty);
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct VarTyi<'a> {
    hint: &'a str,
    inner: Ref<'a, Cell<VarState<'a>>>,
}

#[derive(Copy, Clone)]
enum VarState<'a> {
    Link(Tyi<'a>),
    Unbound(Level),
}

impl<'a> VarTyi<'a> {
    fn new(hint: &str, context: TyiContext<'a>) -> Self {
        let id = context.fresh_id();

        let hint = context.bump.alloc_str(&format!("{}_{:x}", hint, id));
        let level = context.level.get();
        VarTyi {
            hint,
            inner: Ref::new(context.bump.alloc(Cell::new(VarState::Unbound(level)))),
        }
    }

    pub(crate) fn try_unwrap(self) -> Option<Tyi<'a>> {
        match self.inner.get() {
            VarState::Link(ty) => Some(ty),
            VarState::Unbound(..) => None,
        }
    }

    fn escape_to(self, level: Level) {
        match self.inner.get() {
            VarState::Link(ty) => ty.escape_to(level),
            VarState::Unbound(my_level) => {
                if my_level > level {
                    self.inner.set(VarState::Unbound(level));
                }
            }
        }
    }

    pub(crate) fn bind(self, ty: Tyi<'a>) {
        let my_level = match self.inner.get() {
            VarState::Unbound(it) => it,
            VarState::Link(_) => unreachable!(),
        };

        // 型変数のレベルを引き下げる。
        ty.escape_to(my_level);

        self.inner.set(VarState::Link(ty));
    }
}

impl<'a> Debug for VarTyi<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.inner.get() {
            VarState::Link(ty) => Debug::fmt(&ty, f),
            VarState::Unbound(level) => write!(f, "{}'{}", self.hint, level),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FnTyi<'a> {
    inner: Ref<'a, FnTyiData<'a>>,
}

#[derive(Eq, PartialEq, Hash)]
struct FnTyiData<'a> {
    param_tys: BumpaloVec<'a, Tyi<'a>>,
    result_ty: Tyi<'a>,
}

impl<'a> FnTyi<'a> {
    pub(crate) fn new(
        param_tys: impl IntoIterator<Item = Tyi<'a>>,
        result_ty: Tyi<'a>,
        context: TyiContext<'a>,
    ) -> Self {
        let param_tys = BumpaloVec::from_iter_in(param_tys, context.bump);
        let ty = FnTyiData {
            param_tys,
            result_ty,
        };
        FnTyi {
            inner: Ref::new(context.bump.alloc(ty)),
        }
    }

    pub(crate) fn param_tys(self) -> &'a [Tyi<'a>] {
        &self.inner.as_ref().param_tys
    }

    pub(crate) fn result_ty(self) -> Tyi<'a> {
        self.inner.result_ty
    }

    fn escape_to(self, level: Level) {
        for param_ty in self.param_tys() {
            param_ty.escape_to(level);
        }

        self.result_ty().escape_to(level);
    }
}

impl<'a> Debug for FnTyi<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.inner.param_tys.as_slice() {
            [] => {
                write!(f, "() -> ")?;
            }
            [param_ty] => {
                write!(f, "(")?;
                Debug::fmt(param_ty, f)?;
                write!(f, ") -> ")?;
            }
            _ => {
                let mut t = f.debug_tuple("");
                for param_ty in &self.inner.param_tys {
                    t.field(param_ty);
                }
                t.finish()?;
                write!(f, " -> ")?;
            }
        }

        Debug::fmt(&self.inner.result_ty, f)
    }
}

/// 型推論中の型の表現。
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum Tyi<'a> {
    /// プリミティブ型 (unit, int など)
    Prim(&'static str),

    /// 型変数
    Var(VarTyi<'a>),

    /// 関数型
    Fn(FnTyi<'a>),
}

impl<'a> Tyi<'a> {
    /// この型に含まれる型変数のレベルを level まで下げる。
    pub(crate) fn escape_to(self, level: Level) {
        match self {
            Tyi::Prim(_) => {}
            Tyi::Var(ty) => ty.escape_to(level),
            Tyi::Fn(ty) => ty.escape_to(level),
        }
    }
}

impl<'a> Debug for Tyi<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Tyi::Prim(name) => write!(f, "{}", name),
            Tyi::Var(ty) => Debug::fmt(ty, f),
            Tyi::Fn(ty) => Debug::fmt(ty, f),
        }
    }
}

struct TySubst<'a> {
    map: HashMap<VarTyi<'a>, Tyi<'a>>,
    context: TyiContext<'a>,
}

impl<'a> TySubst<'a> {
    fn subst(&self, ty: Tyi<'a>) -> Tyi<'a> {
        match ty {
            Tyi::Prim(_) => ty,
            Tyi::Var(v) => {
                if let Some(ty) = v.try_unwrap() {
                    return self.subst(ty);
                }

                if let Some(ty) = self.map.get(&v) {
                    return *ty;
                }

                ty
            }
            Tyi::Fn(ty) => Tyi::Fn(FnTyi::new(
                ty.param_tys().iter().map(|&ty| self.subst(ty)),
                self.subst(ty.result_ty()),
                self.context,
            )),
        }
    }
}

/// シンボルにつく型。
#[derive(Debug)]
pub(crate) enum TyScheme<'a> {
    Mono(Tyi<'a>),
    Poly {
        vars: Vec<VarTyi<'a>>,
        body: Tyi<'a>,
    },
}

impl<'a> TyScheme<'a> {
    fn do_gen(
        ty: Tyi<'a>,
        vars: &RefCell<HashSet<VarTyi<'a>>>,
        context: TyiContext<'a>,
    ) -> Tyi<'a> {
        match ty {
            Tyi::Prim(_) => ty,
            Tyi::Var(v) => match v.inner.get() {
                VarState::Link(ty) => Self::do_gen(ty, vars, context),
                VarState::Unbound(level) => {
                    if level > context.level.get() {
                        vars.borrow_mut().insert(v);
                    }
                    ty
                }
            },
            Tyi::Fn(ty) => Tyi::Fn(FnTyi::new(
                ty.param_tys()
                    .iter()
                    .map(|ty| Self::do_gen(*ty, vars, context)),
                Self::do_gen(ty.result_ty(), vars, context),
                context,
            )),
        }
    }

    fn generalize(ty: Tyi<'a>, context: TyiContext<'a>) -> Self {
        let level = context.level.get();
        let vars = RefCell::default();
        let body = Self::do_gen(ty, &vars, context);

        let vars = RefCell::into_inner(vars).into_iter().collect::<Vec<_>>();
        if vars.is_empty() {
            TyScheme::Mono(body)
        } else {
            TyScheme::Poly { vars, body }
        }
    }

    fn instantiate(&self, context: TyiContext<'a>) -> Tyi<'a> {
        match self {
            TyScheme::Mono(ty) => *ty,
            TyScheme::Poly { vars, body } => {
                let fresh_var = |s| Tyi::Var(VarTyi::new(s, context));
                let subst = TySubst {
                    map: vars.iter().map(|&var| (var, fresh_var(var.hint))).collect(),
                    context,
                };
                subst.subst(*body)
            }
        }
    }
}

struct Assignable<'a> {
    phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> Assignable<'a> {
    fn new() -> Self {
        Self {
            phantom: std::marker::PhantomData,
        }
    }

    fn on_assign(&self, t: Tyi<'a>, s: Tyi<'a>) {
        match (t, s) {
            (Tyi::Var(t), Tyi::Var(s)) if t == s => return,

            (Tyi::Var(t), s) => match t.try_unwrap() {
                Some(t) => self.on_assign(t, s),
                None => {
                    // TODO: fix level
                    t.bind(s);
                }
            },

            (t, Tyi::Var(s)) => match s.try_unwrap() {
                Some(s) => self.on_assign(t, s),
                None => {
                    // TODO: fix level
                    s.bind(t);
                }
            },

            (Tyi::Prim(t), Tyi::Prim(s)) if t == s => return,
            (Tyi::Fn(t), Tyi::Fn(s)) => {
                if t.param_tys().len() != s.param_tys().len() {
                    panic!("arity mismatch")
                }

                for (&tp, &sp) in t.param_tys().iter().zip(s.param_tys()) {
                    self.on_assign(sp, tp);
                }
                self.on_assign(t.result_ty(), s.result_ty());
            }

            (Tyi::Prim(_), _) | (Tyi::Fn(_), _) => panic!("type mismatch {:?} <- {:?}", t, s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    thread_local! {
        static BUMP: &'static Bump = Box::leak(Box::new(Bump::new()));
    }

    type Var<'a> = &'a str;

    #[derive(Copy, Clone)]
    enum Term<'a> {
        Var(Var<'a>),
        App([&'a Term<'a>; 2]),
        Fn(Var<'a>, &'a Term<'a>),
        Let(Var<'a>, &'a Term<'a>, &'a Term<'a>),
    }

    struct TyInference<'a> {
        last_id: usize,
        env: HashMap<&'a str, TyiSymbol>,
        context: TyiContext<'a>,
    }

    impl<'a> TyInference<'a> {
        fn new(bump: &'a Bump) -> Self {
            TyInference {
                last_id: 0,
                env: HashMap::new(),
                context: TyiContext {
                    last_id: bump.alloc(Cell::new(1)),
                    map: bump.alloc(RefCell::default()),
                    level: bump.alloc(Cell::new(1)),
                    bump,
                },
            }
        }

        fn infer(&mut self, term: Term<'a>) -> Tyi<'a> {
            match term {
                Term::Var(v) => {
                    let symbol = self.env[v];
                    let map = self.context.map.borrow();
                    let scheme = map.get(&symbol).unwrap();
                    scheme.instantiate(self.context)
                }
                Term::App([f, a]) => {
                    let f_ty = self.infer(*f);
                    let a_ty = self.infer(*a);

                    let app_ty = Tyi::Var(VarTyi::new("app", self.context));
                    Assignable::new().on_assign(
                        f_ty,
                        Tyi::Fn(FnTyi::new(std::iter::once(a_ty), app_ty, self.context)),
                    );
                    app_ty
                }
                Term::Fn(v, body) => {
                    let var_id = {
                        self.last_id += 1;
                        self.last_id
                    };
                    self.env.insert(v, var_id);
                    let ty = {
                        self.context.add_symbol(v, var_id, None);
                        let arg_ty = self.context.map.borrow()[&var_id].instantiate(self.context);
                        let body_ty = self.infer(*body);
                        Tyi::Fn(FnTyi::new(std::iter::once(arg_ty), body_ty, self.context))
                    };
                    self.env.remove(v);
                    ty
                }
                Term::Let(v, init, body) => {
                    self.context.level_up();
                    let init_ty = self.infer(*init);
                    self.context.level_down();
                    let symbol_ty = TyScheme::generalize(init_ty, self.context);

                    let var_id = {
                        self.last_id += 1;
                        self.last_id
                    };
                    self.env.insert(v, var_id);
                    self.context.add_symbol(v, var_id, Some(symbol_ty));
                    let ty = self.infer(*body);
                    if self.context.level.get() > 1 {
                        self.env.remove(v);
                    }
                    ty
                }
            }
        }
    }

    fn do_test_infer(term: Term, expect: Expect) {
        BUMP.with(|bump| {
            let mut inference = TyInference::new(bump);
            inference.env.insert("succ", 100);
            inference.context.add_symbol(
                "succ",
                100,
                Some(TyScheme::Mono(Tyi::Fn(FnTyi::new(
                    vec![Tyi::Prim("int")],
                    Tyi::Prim("int"),
                    inference.context,
                )))),
            );

            let ty = inference.infer(term);

            expect.assert_eq(&format!("{:?}", ty));
        });
    }

    fn do_test_infer_scheme(term: Term, v: &str, expect: Expect) {
        BUMP.with(|bump| {
            let mut inference = TyInference::new(bump);
            inference.infer(term);

            let symbol = inference.env[v];
            let map = inference.context.map.borrow();
            let scheme = &map[&symbol];

            expect.assert_eq(&format!("{:?}", scheme));
        });
    }

    fn var(v: &'static str) -> Term<'static> {
        Term::Var(v)
    }

    fn app(f: Term<'static>, x: Term<'static>) -> Term<'static> {
        BUMP.with(|bump| Term::App([bump.alloc(f), bump.alloc(x)]))
    }

    fn fun(v: &'static str, body: Term<'static>) -> Term<'static> {
        BUMP.with(|bump| Term::Fn(v, bump.alloc(body)))
    }

    fn let_in(v: &'static str, init: Term<'static>, body: Term<'static>) -> Term<'static> {
        BUMP.with(|bump| Term::Let(v, bump.alloc(init), bump.alloc(body)))
    }

    // \x. x
    #[test]
    fn test_infer_x_x() {
        do_test_infer(fun("x", var("x")), expect![[r#"(x_2'1) -> x_2'1"#]]);
    }

    // \x. \y. x y
    #[test]
    fn test_infer_x_y_xy() {
        do_test_infer(
            fun("x", fun("y", app(var("x"), var("y")))),
            expect![[r#"((y_3'1) -> app_4'1) -> (y_3'1) -> app_4'1"#]],
        );
    }

    // let id = \x. x
    #[test]
    fn test_infer_id() {
        do_test_infer_scheme(
            let_in("id", fun("x", var("x")), var("id")),
            "id",
            expect![[r#"Poly { vars: [x_2'2], body: (x_2'2) -> x_2'2 }"#]],
        );
    }

    // let id = \x. (let y = x in y)
    #[test]
    fn test_infer_id2() {
        do_test_infer_scheme(
            let_in("id", fun("x", let_in("y", var("x"), var("y"))), var("id")),
            "id",
            expect![[r#"Poly { vars: [x_2'2], body: (x_2'2) -> x_2'2 }"#]],
        );
    }

    // \y. (let id = \x. x in id (id y))
    #[test]
    fn test_infer_id3() {
        do_test_infer(
            fun(
                "y",
                let_in(
                    "id",
                    fun("x", var("x")),
                    app(var("id"), app(var("id"), var("y"))),
                ),
            ),
            expect![[r#"(app_7'1) -> app_7'1"#]],
        );
    }

    // \x. x x
    #[test]
    #[should_panic(expected = "mismatch")]
    fn test_infer_error() {
        do_test_infer(app(var("succ"), var("succ")), expect![[]]);
    }
}
