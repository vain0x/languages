use crate::*;
use std::collections::BTreeSet;

static PRIMS: &[(&str, Prim)] = &[
    ("read_int", Prim::ReadInt),
    ("println_int", Prim::PrintLnInt),
];

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

    fn current_fun_mut(&mut self) -> &mut FunDef {
        self.sema.funs.get_mut(&self.current_fun_id).unwrap()
    }

    fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.sema.syntax.exps[&exp_id]
    }

    fn on_pat(&mut self, exp_id: ExpId) {
        self.on_exp(exp_id, ExpMode::Pat);
    }

    fn on_ref(&mut self, exp_id: ExpId) {
        self.on_exp(exp_id, ExpMode::Ref);
    }

    fn on_val(&mut self, exp_id: ExpId) {
        self.on_exp(exp_id, ExpMode::Val);
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
        self.on_val(root_exp_id);
    }
}

impl ShareSyntax for SemanticAnalyzer {
    fn share_syntax(&self) -> Rc<Syntax> {
        self.sema.syntax.clone()
    }
}

impl ExpVisitor for SemanticAnalyzer {
    type Mode = ExpMode;
    type Output = ();

    fn on_err(&mut self, _: ExpId, _: ExpMode, _: MsgId) {}

    fn on_int(&mut self, _: ExpId, mode: ExpMode, _: i64) {
        match mode {
            ExpMode::Pat | ExpMode::Ref => unimplemented!(),
            ExpMode::Val => {}
        }
    }

    fn on_str(&mut self, _: ExpId, mode: ExpMode, _: &str) {
        match mode {
            ExpMode::Pat | ExpMode::Ref => unimplemented!(),
            ExpMode::Val => {}
        }
    }

    fn on_ident(&mut self, exp_id: ExpId, mode: ExpMode, name: &str) {
        match mode {
            ExpMode::Pat => {
                let symbol_id = self.add_local(name.to_string());
                self.env.insert(name.to_string(), symbol_id);
                self.sema.exp_symbols.insert(exp_id, symbol_id);
            }
            ExpMode::Ref | ExpMode::Val => {
                if let Some(&symbol_id) = self.env.get(name) {
                    self.sema.exp_symbols.insert(exp_id, symbol_id);

                    match (&self.sema.symbols[&symbol_id].kind, mode) {
                        (_, ExpMode::Pat) => unreachable!(),
                        (SymbolKind::Prim { .. }, ExpMode::Val) => {}
                        (SymbolKind::Prim { .. }, ExpMode::Ref) => unimplemented!(),
                        (SymbolKind::Local { .. }, ExpMode::Val) => {
                            self.sema.exp_vals.insert(exp_id);
                        }
                        (SymbolKind::Local { .. }, ExpMode::Ref) => {}
                    }
                } else {
                    panic!("unknown ident {}", name)
                }
            }
        }
    }

    fn on_call(&mut self, _: ExpId, _: ExpMode, callee: ExpId, args: &[ExpId]) {
        self.on_val(callee);
        for &arg in args {
            self.on_val(arg);
        }
    }

    fn on_bin(&mut self, _: ExpId, _: ExpMode, op: Op, exp_l: ExpId, exp_r: ExpId) {
        match op {
            Op::Set | Op::SetAdd => self.on_ref(exp_l),
            _ => self.on_val(exp_l),
        }
        self.on_val(exp_r);
    }

    fn on_if(&mut self, _: ExpId, _: ExpMode, cond: ExpId, body: ExpId, alt: ExpId) {
        self.on_val(cond);
        self.on_val(body);
        self.on_val(alt);
    }

    fn on_while(&mut self, _: ExpId, _: ExpMode, cond: ExpId, body: ExpId) {
        self.on_val(cond);
        self.on_val(body);
    }

    fn on_let(&mut self, _: ExpId, _: ExpMode, pat: ExpId, init: ExpId) {
        self.on_pat(pat);
        self.on_val(init);
    }

    fn on_semi(&mut self, _: ExpId, _: ExpMode, exps: &[ExpId]) {
        for &exp_id in exps {
            self.on_val(exp_id);
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
            funs: BTreeMap::new(),
            msgs: syntax.msgs.clone(),
        },
        env: BTreeMap::new(),
        current_fun_id: GLOBAL_FUN_ID,
    };
    analyzer.sema();
    analyzer.sema
}
