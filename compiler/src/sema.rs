use crate::*;

#[derive(Clone, Debug)]
pub struct VarDef {
    pub name: String,
    pub index: usize,
    pub kind: VarKind,
}

#[derive(Clone, Debug)]
pub enum VarKind {
    Global,
    Local,
    Param(FunId),
}

#[derive(Clone, Default)]
pub struct FunDef {
    pub name: String,
    pub body: ExpId,
    pub params: Vec<VarId>,
    pub locals: Vec<VarId>,
}

#[derive(Clone, Copy, Debug)]
pub enum FunRef {
    Fun(FunId),
    Prim(Prim),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Prim {
    AddInt,
    SubInt,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    AddStr,
    ToStr,
    ReadStr,
    ReadInt,
    Print,
    PrintLn,
    Begin,
    Cond,
    While,
}

#[derive(Clone, Debug)]
pub enum ExpKind {
    Err(String),
    None,
    Int(i64),
    Str(String),
    Var(VarId),
    App(FunRef),
    Set(VarId),
}

#[derive(Clone, Debug)]
pub struct Exp {
    pub kind: ExpKind,
    pub children: Vec<ExpId>,
}

#[derive(Clone, Default)]
pub struct Sema {
    pub toks: Vec<Tok>,
    pub syns: Vec<Syn>,
    pub vars: Vec<VarDef>,
    pub funs: Vec<FunDef>,
    pub exps: Vec<Exp>,
    pub cur_fun_id: FunId,
}

const PRIMS: &'static [(&'static str, Prim, PrimArity)] = &[
    ("+", Prim::AddInt, PrimArity::Bin),
    ("-", Prim::SubInt, PrimArity::Bin),
    ("*", Prim::Mul, PrimArity::Bin),
    ("/", Prim::Div, PrimArity::Bin),
    ("%", Prim::Mod, PrimArity::Bin),
    ("==", Prim::Eq, PrimArity::Fixed(2)),
    ("!=", Prim::Ne, PrimArity::Fixed(2)),
    ("<", Prim::Lt, PrimArity::Fixed(2)),
    ("<=", Prim::Le, PrimArity::Fixed(2)),
    (">", Prim::Gt, PrimArity::Fixed(2)),
    (">=", Prim::Ge, PrimArity::Fixed(2)),
    ("++", Prim::AddStr, PrimArity::Bin),
    ("to_str", Prim::ToStr, PrimArity::Fixed(1)),
    ("read_str", Prim::ReadStr, PrimArity::Fixed(0)),
    ("read_int", Prim::ReadInt, PrimArity::Fixed(0)),
    ("print", Prim::Print, PrimArity::Infinite),
    ("println", Prim::PrintLn, PrimArity::Infinite),
    ("begin", Prim::Begin, PrimArity::Infinite),
    ("cond", Prim::Cond, PrimArity::Infinite),
    ("while", Prim::While, PrimArity::Fixed(2)),
];

impl HaveSyntaxModel for Sema {
    fn toks(&self) -> &[Tok] {
        &self.toks
    }

    fn syns(&self) -> &[Syn] {
        &self.syns
    }
}

impl FunDef {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

impl Sema {
    fn add_param(&mut self, name: String) {
        let fun = &mut self.funs[self.cur_fun_id];
        let index = fun.params.len();
        let var_id = self.vars.len();

        self.vars.push(VarDef {
            name: name.clone(),
            index,
            kind: VarKind::Param(self.cur_fun_id),
        });

        fun.params.push(var_id);
    }

    fn add_local(&mut self, name: String) -> VarId {
        let fun = &mut self.funs[self.cur_fun_id];
        let kind = if self.cur_fun_id == GLOBAL_FUN_ID {
            VarKind::Global
        } else {
            VarKind::Local
        };
        let index = fun.locals.len();
        let var_id = self.vars.len();

        self.vars.push(VarDef {
            name: name.clone(),
            index,
            kind,
        });
        fun.locals.push(var_id);

        var_id
    }

    fn find_var_id(&self, name: &str) -> Option<VarId> {
        for &fun_id in &[self.cur_fun_id, GLOBAL_FUN_ID] {
            for &var_id in self.funs[fun_id].locals.iter().rev() {
                if self.vars[var_id].name == name {
                    return Some(var_id);
                }
            }

            for &var_id in self.funs[fun_id].params.iter().rev() {
                if self.vars[var_id].name == name {
                    return Some(var_id);
                }
            }
        }
        None
    }

    fn find_fun_id(&self, name: &str) -> Option<FunId> {
        for fun_id in (0..self.funs.len()).rev() {
            if self.funs[fun_id].name == name {
                return Some(fun_id);
            }
        }
        None
    }

    fn add_exp(&mut self, kind: ExpKind, children: Vec<ExpId>) -> ExpId {
        self.exps.push(Exp { kind, children });
        self.exps.len() - 1
    }

    fn add_term(&mut self, kind: ExpKind) -> ExpId {
        self.add_exp(kind, vec![])
    }

    fn add_err(&mut self, message: String) -> ExpId {
        self.add_exp(ExpKind::Err(message), vec![])
    }

    fn on_tok(&mut self, tok_id: TokId) -> ExpId {
        match &self.toks[tok_id] {
            Tok::Err(err) => self.add_err(err.to_owned()),
            Tok::Id(name) => {
                if let Some(var_id) = self.find_var_id(name) {
                    self.add_term(ExpKind::Var(var_id))
                } else if name == "true" {
                    self.add_term(ExpKind::Int(1))
                } else if name == "false" {
                    self.add_term(ExpKind::Int(0))
                } else {
                    panic!("undefined variable {}", name)
                }
            }
            &Tok::Int(value) => self.add_term(ExpKind::Int(value)),
            Tok::Str(value) => self.add_term(ExpKind::Str(value.to_owned())),
            Tok::Pun(_) | Tok::Eof => unreachable!(),
        }
    }

    fn on_children(&mut self, syns: &[SynId]) -> Vec<ExpId> {
        let mut children = vec![];
        for i in 0..syns.len() {
            children.push(self.on_exp(syns[i]));
        }
        children
    }

    fn on_app(&mut self, name: &str, syns: Vec<SynId>) -> ExpId {
        if name == "def" {
            let sig_syns = match self.syn_as_app(syns[0]) {
                Some(syns) => syns.to_owned(),
                _ => panic!("first arg of def must be app"),
            };
            let name = match self.syn_as_id(sig_syns[0]) {
                Some(name) => name.to_owned(),
                None => panic!("invalid token for fun name"),
            };

            let cur_fun_id = self.cur_fun_id;
            self.funs.push(FunDef {
                name: name,
                ..FunDef::default()
            });
            self.cur_fun_id = self.funs.len() - 1;

            for i in 1..sig_syns.len() {
                let name = match self.syn_as_id(sig_syns[i]) {
                    Some(name) => name.to_owned(),
                    None => panic!("invalid token for param name"),
                };
                self.add_param(name);
            }

            let body = self.on_exp(syns[1]);

            self.funs[self.cur_fun_id].body = body;
            self.cur_fun_id = cur_fun_id;
            return self.add_term(ExpKind::None);
        }

        if name == "let" {
            let name = match &self.syn_as_id(syns[0]) {
                &Some(name) => name.to_owned(),
                None => panic!("expected name as first argument of let"),
            };
            let r = self.on_exp(syns[1]);
            let var_id = self.add_local(name);
            return self.add_exp(ExpKind::Set(var_id), vec![r]);
        }

        if name == "set" {
            let var_id = match self
                .syn_as_id(syns[0])
                .map(|name| (name, self.find_var_id(name)))
            {
                Some((_, Some(var_id))) => var_id,
                Some((name, None)) => return self.add_err(format!("Undefined variable {}", name)),
                None => return self.add_err("First arg of set must be a variable".into()),
            };
            let r = self.on_exp(syns[1]);
            return self.add_exp(ExpKind::Set(var_id), vec![r]);
        }

        for i in 0..PRIMS.len() {
            if name != PRIMS[i].0 {
                continue;
            }

            let fun_ref = FunRef::Prim(PRIMS[i].1);
            match PRIMS[i].2 {
                PrimArity::Bin if syns.len() == 0 => {
                    return self.add_err("Expected 1+ arguments".into());
                }
                PrimArity::Bin => {
                    let mut l = self.on_exp(syns[0]);
                    for i in 1..syns.len() {
                        let r = self.on_exp(syns[i]);
                        let t = self.add_exp(ExpKind::App(fun_ref), vec![l, r]);
                        l = t;
                    }
                    return l;
                }
                PrimArity::Fixed(arity) if syns.len() != arity => {
                    return self.add_err(format!("Expected {} arguments", arity));
                }
                PrimArity::Fixed(_) | PrimArity::Infinite => {
                    let children = self.on_children(&syns);
                    return self.add_exp(ExpKind::App(fun_ref), children);
                }
            }
        }

        let fun_ref = match self.find_fun_id(name) {
            Some(fun_id) => FunRef::Fun(fun_id),
            None => panic!("Unknown function name {}", name),
        };

        let children = self.on_children(&syns);
        self.add_exp(ExpKind::App(fun_ref), children)
    }

    fn on_exp(&mut self, syn_id: SynId) -> ExpId {
        match &self.syns[syn_id] {
            Syn::Err(err, _) => self.add_err(err.to_owned()),
            &Syn::Val(tok_id) => self.on_tok(tok_id),
            Syn::App(syns) => match &self.syns[syns[0]] {
                &Syn::Val(tok_id) => match &self.toks[tok_id] {
                    Tok::Id(head) => self.on_app(&head.to_owned(), syns[1..].to_owned()),
                    tok => panic!("{:?} callee must be identifier", &tok),
                },
                syn => panic!("callee must be identifier {:?}", syn),
            },
        }
    }

    pub fn sema(&mut self) {
        let entry_syn_id = self.syns.len() - 1;

        // Entry function.
        self.funs.push(FunDef {
            name: "main".into(),
            ..FunDef::default()
        });

        let body = self.on_exp(entry_syn_id);

        self.funs[GLOBAL_FUN_ID].body = body;
    }
}
