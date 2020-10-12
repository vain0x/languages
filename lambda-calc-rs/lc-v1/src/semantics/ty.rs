use super::*;

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum Ty<'a> {
    Unit,
    Bool,
    Int,
    Fn(FnTy<'a>),
}

impl<'a> Debug for Ty<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Ty::Unit => write!(f, "unit"),
            Ty::Bool => write!(f, "bool"),
            Ty::Int => write!(f, "int"),
            Ty::Fn(ty) => Debug::fmt(ty, f),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) struct FnTy<'a> {
    pub(crate) params: &'a [Ty<'a>],
    pub(crate) result: &'a Ty<'a>,
}

impl<'a> Debug for FnTy<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.params.len() {
            0 => write!(f, "fn()")?,
            1 => {
                write!(f, "fn(")?;
                Debug::fmt(&self.params[0], f)?;
                write!(f, ")")?;
            }
            _ => {
                let mut tuple = f.debug_tuple("fn");
                for param in self.params {
                    tuple.field(param);
                }
                tuple.finish()?;
            }
        }

        write!(f, " -> ")?;
        Debug::fmt(self.result, f)
    }
}
