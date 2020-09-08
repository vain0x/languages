use super::{scope::Scope, scope_kind::ScopeKind};

pub(crate) struct ScopeChain<'a> {
    pub(super) inner: Vec<Scope<'a>>,
}

impl<'a> ScopeChain<'a> {
    pub(crate) fn new() -> Self {
        let inner = vec![Scope::new_prim(), Scope::new(ScopeKind::Root)];
        Self { inner }
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    pub(crate) fn last_mut(&mut self) -> &mut Scope<'a> {
        self.inner.last_mut().unwrap()
    }
}
