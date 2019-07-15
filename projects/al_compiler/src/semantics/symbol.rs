#[derive(Clone, Copy, Debug)]
pub(crate) enum SymbolKind {
    Var,
    Fn,
}

#[derive(Clone, Debug)]
pub(crate) struct Var {
    ident: String,
}

#[derive(Clone, Debug)]
pub(crate) struct Fn {
    ident: String,
}

pub(crate) struct Symbols {
    vars: Vec<Var>,
    fns: Vec<Fn>
}

impl Var {
    pub(crate) fn new(ident: String) -> Var {
        Var {
            ident
        }
    }

    pub(crate) fn ident(&self) -> &str {
        &self.ident
    }
}

impl Fn {
    pub(crate) fn new(ident: String) -> Fn {
        Fn {
            ident
        }
    }
}

impl Symbols {
    pub(crate) fn new() -> Symbols {
        Symbols {
            vars: vec![],
            fns: vec![],
        }
    }

    pub(crate) fn vars(&self) -> &[Var] {
        &self.vars
    }

    pub(crate) fn find_or_new_var(&mut self, ident: String) -> usize {
        match self.vars.iter().position(|var| var.ident == ident) {
            Some(var_id) => var_id,
            None => {
                let var_id = self.vars.len();
                self.vars.push(Var::new(ident));
                var_id
            }
        }
    }
}
