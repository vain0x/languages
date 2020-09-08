use super::scope_kind::ScopeKind;
use crate::semantics::{prim::Prim, symbol::NSymbol};
use std::collections::HashMap;

pub(crate) struct Scope<'a> {
    pub(super) kind: ScopeKind,
    pub(super) map: HashMap<&'a str, NSymbol>,
    pub(super) local_var_count: usize,
}

impl<'a> Scope<'a> {
    pub(crate) fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            map: HashMap::new(),
            local_var_count: 0,
        }
    }

    pub(crate) fn new_prim() -> Self {
        let mut scope = Scope::new(ScopeKind::Prim);
        for &(prim, name) in Prim::LIST {
            scope.map.insert(name, NSymbol::Prim(prim));
        }
        scope
    }
}
