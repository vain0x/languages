#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum Ty<'a> {
    Unit,
    Bool,
    Int,
    Fn {
        params: &'a [Ty<'a>],
        result: &'a Ty<'a>,
    },
}
