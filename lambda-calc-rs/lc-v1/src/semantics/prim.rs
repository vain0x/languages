#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum Prim {
    IntEq,
    IntAdd,
}

impl Prim {
    pub(crate) const LIST: &'static [(Prim, &'static str)] =
        &[(Prim::IntEq, "int_eq"), (Prim::IntAdd, "int_add")];
}
