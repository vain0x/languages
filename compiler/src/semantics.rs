use crate::*;

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

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Prim {
    ByteToInt,
    IntToByte,
    MemAlloc,
    ReadInt,
    PrintLnInt,
    Print,
}

#[derive(Clone, Debug)]
pub(crate) struct VarDef {
    pub name: String,
    pub ty: Ty,
    pub index: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct FunDef {
    pub name: String,
    pub ty: Ty,
    pub body: ExpId,
    pub symbols: Vec<SymbolKind>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum SymbolKind {
    Prim(Prim),
    Var(VarId),
    Fun(FunId),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SymbolRef<'a> {
    Prim(Prim),
    Var(VarId, &'a VarDef),
    Fun(FunId, &'a FunDef),
}

#[derive(Clone, Debug)]
pub(crate) struct Sema {
    pub syntax: Rc<Syntax>,
    pub exp_symbols: BTreeMap<ExpId, SymbolKind>,
    pub exp_vals: BTreeSet<ExpId>,
    pub exp_tys: BTreeMap<ExpId, Ty>,
    pub vars: BTreeMap<VarId, VarDef>,
    pub funs: BTreeMap<FunId, FunDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
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

pub(crate) static PRIMS: &[(&str, Prim)] = &[
    ("byte_to_int", Prim::ByteToInt),
    ("int_to_byte", Prim::IntToByte),
    ("read_int", Prim::ReadInt),
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
            Prim::MemAlloc => Ty::Fun(vec![Ty::Int, Ty::Ptr]),
            Prim::PrintLnInt => Ty::Fun(vec![Ty::Int, Ty::Unit]),
            Prim::ReadInt => Ty::Fun(vec![Ty::Int]),
            Prim::Print => Ty::Fun(vec![Ty::Ptr, Ty::Int, Ty::Unit]),
        }
    }
}

impl SymbolKind {
    pub fn is_var(&self) -> bool {
        match self {
            SymbolKind::Var(_) => true,
            _ => false,
        }
    }
}

impl SymbolRef<'_> {
    pub fn kind(&self) -> SymbolKind {
        match self {
            &SymbolRef::Prim(prim) => SymbolKind::Prim(prim),
            &SymbolRef::Var(var_id, _) => SymbolKind::Var(var_id),
            &SymbolRef::Fun(fun_id, _) => SymbolKind::Fun(fun_id),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            SymbolRef::Prim(prim) => prim.text(),
            SymbolRef::Var(_, var_def) => &var_def.name,
            SymbolRef::Fun(_, fun_def) => &fun_def.name,
        }
    }

    pub fn get_ty(&self) -> Ty {
        match self {
            SymbolRef::Prim(prim) => prim.get_ty(),
            SymbolRef::Var(_, var_def) => var_def.ty.to_owned(),
            SymbolRef::Fun(_, fun_def) => fun_def.ty.to_owned(),
        }
    }
}

impl Sema {
    pub(crate) fn is_successful(&self) -> bool {
        self.msgs.iter().all(|(_, msg)| msg.is_successful())
    }
}

impl BorrowMutMsgs for Sema {
    fn msgs_mut(&mut self) -> &mut Msgs {
        &mut self.msgs
    }
}

pub(crate) const GLOBAL_FUN_ID: FunId = FunId(0);
