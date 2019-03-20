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

    pub(crate) fn make_fun<T: IntoIterator<Item = Ty>>(args: T, result: Ty) -> Ty {
        Ty::Fun(args.into_iter().chain(iter::once(result)).collect())
    }

    pub(crate) fn size_of(&self) -> Option<usize> {
        match self {
            Ty::Err | Ty::Var(_) | Ty::Fun(_) => None,
            Ty::Unit | Ty::Byte => Some(1),
            Ty::Int | Ty::Ptr => Some(8),
        }
    }

    pub(crate) fn primitive_ty_names() -> Vec<String> {
        const NAMES: &[&str] = &["unit", "byte", "int"];
        NAMES.iter().map(|name| name.to_string()).collect()
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Ty::Err => write!(out, "error")?,
            Ty::Var(_) => write!(out, "_")?,
            Ty::Unit => write!(out, "()")?,
            Ty::Byte => write!(out, "byte")?,
            Ty::Int => write!(out, "int")?,
            Ty::Ptr => write!(out, "[byte]")?,
            Ty::Fun(tys) => {
                write!(out, "|")?;
                for i in 0..tys.len() {
                    if i + 1 == tys.len() {
                        write!(out, "| ")?;
                    } else if i > 0 {
                        write!(out, ", ")?;
                    }
                    tys[i].fmt(out)?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ty_display() {
        assert_eq!(format!("{}", Ty::Byte), "byte");

        assert_eq!(format!("{}", Ty::make_fun(vec![], Ty::Int)), "|| int");

        assert_eq!(
            format!("{}", Ty::make_fun(vec![Ty::Int, Ty::Byte], Ty::Unit)),
            "|int, byte| ()"
        );
    }
}
