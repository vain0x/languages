use crate::semantics::{local_symbol::NLocalSymbol, symbol::NSymbol};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub(crate) struct NameResolution {
    pub(crate) ident_symbols: HashMap<usize, NSymbol>,
    // キーは `fn` キーワードの id
    pub(crate) fn_escapes: HashMap<usize, HashSet<NLocalSymbol>>,
}
