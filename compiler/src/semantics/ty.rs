use crate::syntax::*;
use crate::Id;
use std::collections::{BTreeMap, BTreeSet};
use std::iter;

pub(crate) type TyId = Id<Ty>;

/// Type constructor.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum TyCon {
    Unit,
    Byte,
    Int,
    Slice,
    Fun,
}

#[derive(Clone, Debug)]
pub(crate) enum Ty {
    #[allow(unused)]
    Err,
    Meta(TyId),
    Con(TyCon, Vec<Ty>),
}

#[derive(Clone, Debug)]
pub(crate) struct TyScheme {
    meta: Vec<TyId>,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub(crate) struct TyDef {
    pub(super) ty: Option<Ty>,
    pub(super) exp_id: ExpId,
}

impl Ty {
    pub(crate) fn unit() -> Ty {
        Ty::Con(TyCon::Unit, Vec::new())
    }

    pub(crate) fn byte() -> Ty {
        Ty::Con(TyCon::Byte, Vec::new())
    }

    pub(crate) fn int() -> Ty {
        Ty::Con(TyCon::Int, Vec::new())
    }

    pub(crate) fn ptr(inner_ty: Ty) -> Ty {
        Ty::Con(TyCon::Slice, vec![inner_ty])
    }

    pub(crate) fn make_str() -> Ty {
        Ty::ptr(Ty::byte())
    }

    pub(crate) fn make_fun(args: impl IntoIterator<Item = Ty>, result: Ty) -> Ty {
        Ty::Con(
            TyCon::Fun,
            args.into_iter().chain(iter::once(result)).collect(),
        )
    }

    pub(crate) fn replace_with(self, replacer: &mut impl Fn(TyId) -> Ty) -> Ty {
        let mut ty_ids = BTreeSet::new();
        let mut ty = self;

        loop {
            match ty {
                Ty::Err => return Ty::Err,
                Ty::Meta(ty_id) if ty_ids.contains(&ty_id) => return Ty::Meta(ty_id),
                Ty::Meta(ty_id) => {
                    ty_ids.insert(ty_id);
                    ty = replacer(ty_id);
                }
                Ty::Con(ty_con, tys) => {
                    let tys = tys
                        .into_iter()
                        .map(|ty| ty.replace_with(replacer))
                        .collect();
                    return Ty::Con(ty_con, tys);
                }
            }
        }
    }

    pub(crate) fn size_of(&self) -> Option<usize> {
        match self {
            Ty::Con(TyCon::Unit, _) | Ty::Con(TyCon::Byte, _) => Some(1),
            Ty::Con(TyCon::Int, _) | Ty::Con(TyCon::Slice, _) => Some(8),
            _ => None,
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
            Ty::Err => write!(out, "error"),
            Ty::Meta(_) => write!(out, "_"),
            Ty::Con(TyCon::Unit, _) => write!(out, "unit"),
            Ty::Con(TyCon::Byte, _) => write!(out, "byte"),
            Ty::Con(TyCon::Int, _) => write!(out, "int"),
            Ty::Con(TyCon::Slice, _) => write!(out, "[byte]"),
            Ty::Con(TyCon::Fun, tys) => {
                write!(out, "|")?;
                for i in 0..tys.len() {
                    if i + 1 == tys.len() {
                        write!(out, "| ")?;
                    } else if i > 0 {
                        write!(out, ", ")?;
                    }
                    tys[i].fmt(out)?;
                }
                Ok(())
            }
        }
    }
}

impl From<Ty> for TyScheme {
    fn from(ty: Ty) -> TyScheme {
        TyScheme { meta: vec![], ty }
    }
}

impl TyScheme {
    pub(crate) fn instantiate(&self, mut fresh_meta_ty: impl FnMut() -> Ty) -> Ty {
        let map = self
            .meta
            .iter()
            .map(|&ty_id| (ty_id, fresh_meta_ty()))
            .collect::<BTreeMap<_, _>>();
        self.ty
            .clone()
            .replace_with(&mut |ty_id| map.get(&ty_id).cloned().unwrap_or(Ty::Meta(ty_id)))
    }

    pub(crate) fn generalize(ty: Ty) -> TyScheme {
        fn walk(ty: &Ty, meta: &mut Vec<TyId>) {
            match ty {
                Ty::Err => {}
                &Ty::Meta(ty_id) => {
                    meta.push(ty_id);
                }
                Ty::Con(_, tys) => {
                    for ty in tys {
                        walk(ty, meta);
                    }
                }
            }
        }

        let mut meta = Vec::new();
        walk(&ty, &mut meta);
        meta.sort();
        meta.dedup();

        TyScheme { meta, ty }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ty_display() {
        assert_eq!(format!("{}", Ty::byte()), "byte");

        assert_eq!(format!("{}", Ty::make_fun(vec![], Ty::int())), "|| int");

        assert_eq!(
            format!("{}", Ty::make_fun(vec![Ty::int(), Ty::byte()], Ty::unit())),
            "|int, byte| unit"
        );
    }
}
