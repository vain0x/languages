use super::ty::Ty;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum PrimTy {
    Unit,
    Bool,
    Int,
}

impl PrimTy {
    pub(crate) fn from_str(name: &str) -> Option<PrimTy> {
        let ty = match name {
            "unit" => PrimTy::Unit,
            "bool" => PrimTy::Bool,
            "int" => PrimTy::Int,
            _ => return None,
        };
        Some(ty)
    }

    pub(crate) fn to_ty(self) -> Ty<'static> {
        match self {
            PrimTy::Unit => Ty::Unit,
            PrimTy::Bool => Ty::Bool,
            PrimTy::Int => Ty::Int,
        }
    }
}
