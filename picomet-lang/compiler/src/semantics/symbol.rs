use super::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum SymbolKind {
    Prim(Prim),
    Var(VarId),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SymbolRef<'a> {
    Prim(Prim),
    Var(VarId, &'a VarDef),
}

impl SymbolKind {
    pub fn is_var(&self) -> bool {
        match self {
            SymbolKind::Var(_) => true,
            _ => false,
        }
    }
}

impl SymbolRef<'_> {
    pub fn kind(&self) -> SymbolKind {
        match self {
            &SymbolRef::Prim(prim) => SymbolKind::Prim(prim),
            &SymbolRef::Var(var_id, _) => SymbolKind::Var(var_id),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            SymbolRef::Prim(prim) => prim.text(),
            SymbolRef::Var(_, var_def) => &var_def.name,
        }
    }

    pub fn ty_scheme(&self) -> TyScheme {
        match self {
            SymbolRef::Prim(prim) => prim.ty_scheme(),
            SymbolRef::Var(_, var_def) => match var_def.ty_scheme.clone() {
                Ok(ty_scheme) => ty_scheme,
                Err(ty) => TyScheme::from(ty),
            },
        }
    }

    pub(crate) fn def_exp_id(&self) -> Option<ExpId> {
        match self {
            SymbolRef::Prim(_) => None,
            SymbolRef::Var(_, var_def) => Some(var_def.def_exp_id),
        }
    }
}
