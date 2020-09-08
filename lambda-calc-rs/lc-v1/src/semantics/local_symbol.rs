use super::symbol::NSymbol;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum NLocalSymbol {
    Param { id: usize },
    LocalVar { id: usize },
}

impl NLocalSymbol {
    pub(crate) fn from_symbol(symbol: NSymbol) -> Option<Self> {
        let symbol = match symbol {
            NSymbol::Param { id, .. } => NLocalSymbol::Param { id },
            NSymbol::LocalVar { id, .. } => NLocalSymbol::LocalVar { id },
            _ => return None,
        };
        Some(symbol)
    }
}
