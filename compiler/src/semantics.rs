pub(crate) mod analyze;
pub(crate) mod msg;
pub(crate) mod prim;
pub(crate) mod symbol;
pub(crate) mod ty;

pub(crate) use self::msg::*;
pub(crate) use self::prim::*;
pub(crate) use self::symbol::*;
pub(crate) use self::ty::*;

use crate::syntax::*;
use crate::Id;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

pub(crate) type MsgId = Id<Msg>;
pub(crate) type VarId = Id<VarDef>;
pub(crate) type FunId = Id<FunDef>;

#[derive(Clone, Copy, Debug)]
pub(crate) enum VarKind {
    Global { index: usize },
    Local { index: usize },
    Arg { index: usize },
}

#[derive(Clone, Debug)]
pub(crate) struct VarDef {
    pub name: String,
    pub ty: Ty,
    pub kind: VarKind,
}

#[derive(Clone, Debug)]
pub(crate) struct FunDef {
    pub name: String,
    pub ty: Ty,
    pub bodies: Vec<ExpId>,
    pub symbols: Vec<SymbolKind>,
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

pub(crate) const GLOBAL_FUN_ID: FunId = FunId::new(0);
