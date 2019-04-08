use super::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Prim {
    SliceLen,
    MemAlloc,
    ReadInt,
    ReadStr,
    PrintLnInt,
    Print,
}

pub(crate) static PRIMS: &[(&str, Prim)] = &[
    ("slice_len", Prim::SliceLen),
    ("read_int", Prim::ReadInt),
    ("read_str", Prim::ReadStr),
    ("mem_alloc", Prim::MemAlloc),
    ("println_int", Prim::PrintLnInt),
    ("print", Prim::Print),
];

impl Prim {
    pub fn text(self) -> &'static str {
        PRIMS.iter().find(|&&(_, prim)| prim == self).unwrap().0
    }

    pub(crate) fn ty_scheme(self) -> TyScheme {
        match self {
            Prim::SliceLen => TyScheme::generalize(Ty::make_fun(
                vec![Ty::ptr(Ty::Meta(TyId::new(0)))],
                Ty::int(),
            )),
            Prim::MemAlloc => TyScheme::generalize(Ty::make_fun(
                vec![Ty::int()],
                Ty::ptr(Ty::Meta(TyId::new(0))),
            )),
            Prim::PrintLnInt => TyScheme::generalize(Ty::make_fun(vec![Ty::int()], Ty::unit())),
            Prim::ReadInt => TyScheme::generalize(Ty::make_fun(vec![], Ty::int())),
            Prim::ReadStr => TyScheme::generalize(Ty::make_fun(vec![], Ty::make_str())),
            Prim::Print => TyScheme::generalize(Ty::make_fun(vec![Ty::make_str()], Ty::unit())),
        }
    }
}
