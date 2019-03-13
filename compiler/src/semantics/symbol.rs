use super::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum SymbolKind {
    Prim(Prim),
    Var(VarId),
    Fun(FunId),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SymbolRef<'a> {
    Prim(Prim),
    Var(VarId, &'a VarDef),
    Fun(FunId, &'a FunDef),
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
            &SymbolRef::Fun(fun_id, _) => SymbolKind::Fun(fun_id),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            SymbolRef::Prim(prim) => prim.text(),
            SymbolRef::Var(_, var_def) => &var_def.name,
            SymbolRef::Fun(_, fun_def) => &fun_def.name,
        }
    }

    pub fn get_ty(&self) -> Ty {
        match self {
            SymbolRef::Prim(prim) => prim.get_ty(),
            SymbolRef::Var(_, var_def) => var_def.ty.to_owned(),
            SymbolRef::Fun(_, fun_def) => fun_def.ty(),
        }
    }
}
