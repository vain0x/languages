#[derive(Clone, Copy, Debug)]
pub(crate) enum SymbolKind {
    Var,
    Fun,
}

#[derive(Clone, Debug)]
pub(crate) struct Var {
    ident: String,
}

#[derive(Clone, Debug)]
pub(crate) struct Fun {
    ident: String,
}

pub(crate) struct Symbols {
    vars: Vec<Var>,
    funs: Vec<Fun>,
}

impl Var {
    pub(crate) fn new(ident: String) -> Var {
        Var { ident }
    }

    pub(crate) fn ident(&self) -> &str {
        &self.ident
    }
}

impl Fun {
    pub(crate) fn new(ident: String) -> Fun {
        Fun { ident }
    }

    pub(crate) fn ident(&self) -> &str {
        &self.ident
    }
}

impl Symbols {
    pub(crate) fn new() -> Symbols {
        Symbols {
            vars: vec![],
            funs: vec![],
        }
    }

    pub(crate) fn vars(&self) -> &[Var] {
        &self.vars
    }

    pub(crate) fn find(&mut self, ident: &str) -> Option<(SymbolKind, usize)> {
        let find_var = || {
            self.vars
                .iter()
                .position(|var| var.ident() == ident)
                .map(|var_id| (SymbolKind::Var, var_id))
        };
        let find_fun = || {
            self.funs
                .iter()
                .position(|fun| fun.ident() == ident)
                .map(|fun_id| (SymbolKind::Fun, fun_id))
        };
        find_var().or_else(find_fun)
    }

    pub(crate) fn find_or_new_var(&mut self, ident: String) -> usize {
        match self.vars.iter().position(|var| var.ident() == ident) {
            Some(var_id) => var_id,
            None => {
                let var_id = self.vars.len();
                self.vars.push(Var::new(ident));
                var_id
            }
        }
    }

    pub(crate) fn find_fun(&mut self, ident: &str) -> Option<usize> {
        self.funs.iter().position(|fun| fun.ident() == ident)
    }

    pub(crate) fn find_or_new_fun(&mut self, ident: String) -> usize {
        match self.funs.iter().position(|fun| fun.ident() == ident) {
            Some(fun_id) => fun_id,
            None => {
                let fun_id = self.funs.len();
                self.funs.push(Fun::new(ident));
                fun_id
            }
        }
    }
}
