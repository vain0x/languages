use super::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Prim {
    ByteToInt,
    IntToByte,
    SliceLen,
    MemAlloc,
    ReadInt,
    ReadStr,
    PrintLnInt,
    Print,
}

pub(crate) static PRIMS: &[(&str, Prim)] = &[
    ("byte_to_int", Prim::ByteToInt),
    ("int_to_byte", Prim::IntToByte),
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

    pub(crate) fn get_ty(self) -> Ty {
        match self {
            Prim::ByteToInt => Ty::make_fun(vec![Ty::byte()], Ty::int()),
            Prim::IntToByte => Ty::make_fun(vec![Ty::int()], Ty::byte()),
            Prim::SliceLen => Ty::make_fun(vec![Ty::ptr()], Ty::int()),
            Prim::MemAlloc => Ty::make_fun(vec![Ty::int()], Ty::ptr()),
            Prim::PrintLnInt => Ty::make_fun(vec![Ty::int()], Ty::unit()),
            Prim::ReadInt => Ty::make_fun(vec![], Ty::int()),
            Prim::ReadStr => Ty::make_fun(vec![], Ty::ptr()),
            Prim::Print => Ty::make_fun(vec![Ty::ptr()], Ty::unit()),
        }
    }
}
