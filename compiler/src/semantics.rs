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
pub(crate) type LoopId = Id<LoopDef>;

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

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum FunKind {
    Decl(ExpId),
    Def {
        /// A function can have 1+ bodies.
        /// These expressions are combined with `;`.
        bodies: Vec<ExpId>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct FunDef {
    pub name: String,
    pub kind: FunKind,
    pub ty: Ty,
    pub symbols: Vec<SymbolKind>,
}

#[derive(Clone, Debug)]
pub(crate) struct LoopDef {
    pub body: ExpId,
}

#[derive(Clone, Debug)]
pub(crate) struct Sema {
    pub syntax: Rc<Syntax>,

    /// Exp to related symbol mapping.
    pub exp_symbols: BTreeMap<ExpId, SymbolKind>,

    /// Exp to related loop mapping.
    pub exp_loops: BTreeMap<ExpId, LoopId>,

    /// Expressions that may be a reference but is a value.
    pub exp_vals: BTreeSet<ExpId>,

    /// Expressions that may be a range.
    pub exp_ranges: BTreeMap<ExpId, (ExpId, ExpId)>,

    /// Expressions that does nothing on runtime.
    pub exp_decls: BTreeSet<ExpId>,

    /// Type of expressions.
    pub exp_tys: BTreeMap<ExpId, Ty>,

    pub vars: BTreeMap<VarId, VarDef>,
    pub funs: BTreeMap<FunId, FunDef>,
    pub loops: BTreeMap<LoopId, LoopDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

impl FunKind {
    pub(crate) fn is_decl(&self) -> bool {
        match self {
            FunKind::Decl(_) => true,
            _ => false,
        }
    }
}

impl FunDef {
    pub(crate) fn ty(&self) -> Ty {
        self.ty.to_owned()
    }

    pub(crate) fn result_ty(&self) -> Option<&Ty> {
        match &self.ty {
            Ty::Fun(tys) => tys.last(),
            _ => None,
        }
    }

    pub(crate) fn bodies(&self) -> Vec<ExpId> {
        match &self.kind {
            FunKind::Decl(..) => vec![],
            FunKind::Def { bodies } => bodies.to_owned(),
        }
    }
}

impl Sema {
    pub(crate) fn to_doc_msgs(&self) -> Vec<DocMsg> {
        Msg::summarize(self.msgs.values(), &self.syntax).1
    }

    pub(crate) fn is_successful(&self) -> bool {
        self.msgs.iter().all(|(_, msg)| msg.is_successful())
    }

    pub(crate) fn all_fun_ids(&self) -> Vec<FunId> {
        self.funs.keys().cloned().collect()
    }

    pub(crate) fn find_module_by_doc_id(&self, doc_id: DocId) -> Option<(ModuleId, &Module)> {
        self.syntax.find_module_by_doc_id(doc_id)
    }
}

impl BorrowMutMsgs for Sema {
    fn msgs_mut(&mut self) -> &mut Msgs {
        &mut self.msgs
    }
}

pub(crate) const GLOBAL_FUN_ID: FunId = FunId::new(0);
