use super::{
    name_resolution::NameResolution, scope::Scope, scope_chain::ScopeChain, scope_kind::ScopeKind,
};
use crate::{
    context::Context, semantics::local_symbol::NLocalSymbol, semantics::prim_ty::PrimTy,
    semantics::symbol::NSymbol, syntax::syntax_token::SyntaxToken,
};
use std::mem::take;

pub(crate) struct ScopeResolver<'a> {
    symbol_id: usize,
    pub(crate) output: NameResolution,
    scope_chain: ScopeChain<'a>,
    #[allow(unused)]
    context: &'a Context,
}

impl<'a> ScopeResolver<'a> {
    pub(crate) fn new(context: &'a Context) -> Self {
        Self {
            symbol_id: 0,
            output: NameResolution::default(),
            scope_chain: ScopeChain::new(),
            context,
        }
    }

    fn fresh_symbol_id(&mut self) -> usize {
        self.symbol_id += 1;
        self.symbol_id
    }

    pub(crate) fn find_value(&mut self, token: SyntaxToken<'a>) -> Option<NSymbol> {
        let mut outer = false;
        let mut found = None;

        for (i, scope) in self.scope_chain.inner.iter().enumerate().rev() {
            match scope.map.get(token.text).copied() {
                Some(symbol) => {
                    found = Some((i, symbol));
                    break;
                }
                None => {
                    if let ScopeKind::Fn(..) = scope.kind {
                        outer = true;
                    }
                    continue;
                }
            }
        }

        let (i, symbol) = found?;
        if outer {
            if let Some(local_symbol) = NLocalSymbol::from_symbol(symbol) {
                for scope in self.scope_chain.inner[i..].iter_mut() {
                    if let ScopeKind::Fn(fn_id) = scope.kind {
                        self.output
                            .fn_escapes
                            .entry(fn_id)
                            .or_default()
                            .insert(local_symbol);
                    }
                }
            }
        }
        Some(symbol)
    }

    pub(crate) fn after_param(&mut self, name: SyntaxToken<'a>, index: usize) {
        let id = self.fresh_symbol_id();
        let depth = self.scope_chain.len() - 1;
        let symbol = NSymbol::Param { id, depth, index };
        self.scope_chain.last_mut().map.insert(name.text, symbol);
        self.output.ident_symbols.insert(name.index, symbol);
    }

    pub(crate) fn after_name_ty(&mut self, token: SyntaxToken<'a>) {
        let symbol = match PrimTy::from_str(token.text) {
            Some(ty) => NSymbol::PrimTy(ty),
            None => NSymbol::Missing,
        };
        self.output.ident_symbols.insert(token.index, symbol);
    }

    pub(crate) fn after_ident_expr(&mut self, token: SyntaxToken<'a>) {
        let symbol = self.find_value(token).unwrap_or(NSymbol::Missing);
        self.output.ident_symbols.insert(token.index, symbol);
    }

    pub(crate) fn before_block_expr(&mut self) {
        self.scope_chain.inner.push(Scope::new(ScopeKind::Block));
    }

    pub(crate) fn after_block_expr(&mut self) {
        let scope_opt = self.scope_chain.inner.pop();
        assert_eq!(scope_opt.map(|s| s.kind), Some(ScopeKind::Block));
    }

    pub(crate) fn before_fn_expr(&mut self, keyword: SyntaxToken<'a>) {
        self.scope_chain
            .inner
            .push(Scope::new(ScopeKind::Fn(keyword.index)));
    }

    pub(crate) fn after_fn_expr(&mut self, keyword: SyntaxToken<'a>) {
        let scope_opt = self.scope_chain.inner.pop();
        assert_eq!(
            scope_opt.map(|s| s.kind),
            Some(ScopeKind::Fn(keyword.index))
        );
    }

    pub(crate) fn after_let_stmt(&mut self, name_opt: Option<SyntaxToken<'a>>) {
        if let Some(name) = &name_opt {
            let id = self.fresh_symbol_id();
            let is_global = self
                .scope_chain
                .inner
                .iter()
                .rev()
                .skip_while(|s| s.kind == ScopeKind::Block)
                .next()
                .unwrap()
                .kind
                == ScopeKind::Root;

            let depth = self.scope_chain.len() - 1;
            let scope = self.scope_chain.last_mut();
            let index = scope.local_var_count;
            let symbol = if is_global {
                NSymbol::StaticVar { id, depth, index }
            } else {
                NSymbol::LocalVar { id, depth, index }
            };
            scope.map.insert(name.text, symbol);

            self.output.ident_symbols.insert(name.index, symbol);
        }
    }

    pub(crate) fn after_root(&mut self) {
        let scope_opt = self.scope_chain.inner.pop();
        assert_eq!(scope_opt.map(|s| s.kind), Some(ScopeKind::Root));
    }

    pub(crate) fn take_output(&mut self) -> NameResolution {
        take(&mut self.output)
    }
}
