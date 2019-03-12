use crate::syntax::*;
use std::iter;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Ty {
    #[allow(unused)]
    Err,
    Var(ExpId),
    Unit,
    Byte,
    Int,
    Ptr,
    Fun(Vec<Ty>),
}

impl Ty {
    pub(crate) fn make_str() -> Ty {
        Ty::Ptr
    }

    pub(crate) fn make_fun<T: Iterator<Item = Ty>>(args: T, result: Ty) -> Ty {
        Ty::Fun(args.chain(iter::once(result)).collect())
    }

    pub(crate) fn size_of(&self) -> Option<usize> {
        match self {
            Ty::Err | Ty::Var(_) | Ty::Fun(_) => None,
            Ty::Unit | Ty::Byte => Some(1),
            Ty::Int | Ty::Ptr => Some(8),
        }
    }
}
