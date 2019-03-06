use crate::*;
use std::collections::BTreeSet;

static PRIMS: &[(&str, Prim)] = &[
    ("read_int", Prim::ReadInt),
    ("println_int", Prim::PrintLnInt),
];

pub struct Sema {
    pub syntax: Rc<Syntax>,
    pub local_count: usize,
    pub pats: BTreeSet<ExpId>,
    pub symbols: BTreeMap<SymbolId, Symbol>,
    pub exp_symbols: BTreeMap<ExpId, SymbolId>,
    pub msgs: BTreeMap<MsgId, Msg>,
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

    fn add_local(&mut self, name: String) -> SymbolId {
        let index = self.sema.local_count;
        self.sema.local_count += 1;

        let symbol = Symbol {
            kind: SymbolKind::Local { index },
            name,
        };

        let symbol_id = SymbolId(self.sema.symbols.len());
        self.sema.symbols.insert(symbol_id, symbol);
        symbol_id
    }

    fn is_pat(&self, exp_id: ExpId) -> bool {
        self.sema.pats.contains(&exp_id)
    }

    fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.sema.syntax.exps[&exp_id]
    }

    fn on_err(&mut self, _: ExpId, _: MsgId) {}

    fn on_int(&mut self, exp_id: ExpId, _: i64) {
        if self.is_pat(exp_id) {
            panic!("int pattern unimplemented")
        }
    }

    fn on_str(&mut self, exp_id: ExpId, _: &str) {
        if self.is_pat(exp_id) {
            panic!("str pattern unimplemented")
        }
    }

    fn on_ident(&mut self, exp_id: ExpId, name: &str) {
        if self.is_pat(exp_id) {
            let index = self.env.len();
            let symbol_id = self.add_local(name.to_string());
            self.env.insert(name.to_string(), symbol_id);
            self.sema.exp_symbols.insert(exp_id, symbol_id);
        } else {
            if let Some(&symbol_id) = self.env.get(name) {
                self.sema.exp_symbols.insert(exp_id, symbol_id);
            } else {
                panic!("unknown ident {}", name)
            }
        }
    }

    fn on_call(&mut self, _: ExpId, callee: ExpId, args: &[ExpId]) {
        self.on_exp(callee);
        for &arg in args {
            self.on_exp(arg);
        }
    }

    fn on_bin(&mut self, _: ExpId, _: Op, exp_l: ExpId, exp_r: ExpId) {
        self.on_exp(exp_l);
        self.on_exp(exp_r);
    }

    fn on_let(&mut self, _: ExpId, pat: ExpId, init: ExpId) {
        self.on_pat(pat);
        self.on_exp(init);
    }

    fn on_semi(&mut self, _: ExpId, exps: &[ExpId]) {
        for &exp_id in exps {
            self.on_exp(exp_id);
        }
    }

    fn on_exp(&mut self, exp_id: ExpId) {
        let syntax = Rc::clone(&self.sema.syntax);
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            &ExpKind::Err(msg_id) => self.on_err(exp_id, msg_id),
            &ExpKind::Int(value) => self.on_int(exp_id, value),
            ExpKind::Str(value) => self.on_str(exp_id, value),
            ExpKind::Ident(name) => self.on_ident(exp_id, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, *callee, &args),
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, op, l, r),
            &ExpKind::Let { pat, init } => self.on_let(exp_id, pat, init),
            ExpKind::Semi(exps) => self.on_semi(exp_id, &exps),
        }
    }

    fn on_pat(&mut self, exp_id: ExpId) {
        self.sema.pats.insert(exp_id);

        self.on_exp(exp_id);
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

        self.on_exp(self.sema.syntax.root_exp_id);
    }
}

pub fn sema(syntax: Rc<Syntax>) -> Sema {
    let mut analyzer = SemanticAnalyzer {
        sema: Sema {
            syntax: Rc::clone(&syntax),
            local_count: 0,
            pats: BTreeSet::new(),
            symbols: BTreeMap::new(),
            exp_symbols: BTreeMap::new(),
            msgs: syntax.msgs.clone(),
        },
        env: BTreeMap::new(),
        current_fun_id: GLOBAL_FUN_ID,
    };
    analyzer.sema();
    analyzer.sema
}
