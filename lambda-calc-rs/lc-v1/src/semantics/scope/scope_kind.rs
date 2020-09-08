#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ScopeKind {
    Block,
    // `fn` キーワードの id
    Fn(usize),
    Root,
    Prim,
}
