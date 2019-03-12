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
            Prim::ByteToInt => Ty::Fun(vec![Ty::Byte, Ty::Int]),
            Prim::IntToByte => Ty::Fun(vec![Ty::Int, Ty::Byte]),
            Prim::SliceLen => Ty::Fun(vec![Ty::Ptr, Ty::Int]),
            Prim::MemAlloc => Ty::Fun(vec![Ty::Int, Ty::Ptr]),
            Prim::PrintLnInt => Ty::Fun(vec![Ty::Int, Ty::Unit]),
            Prim::ReadInt => Ty::Fun(vec![Ty::Int]),
            Prim::ReadStr => Ty::Fun(vec![Ty::Ptr]),
            Prim::Print => Ty::Fun(vec![Ty::Ptr, Ty::Unit]),
        }
    }
}
