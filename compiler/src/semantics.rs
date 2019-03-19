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
    Rec(ExpId),
    Fun(FunId),
}

#[derive(Clone, Debug)]
pub(crate) struct VarDef {
    pub name: String,
    pub kind: VarKind,
    pub ty: Ty,
    pub def_exp_id: ExpId,
}

#[derive(Clone, Debug)]
pub(crate) struct FunDef {
    pub name: String,
    pub ty: Ty,
    pub symbols: Vec<SymbolKind>,

    /// A function can have 1+ bodies.
    /// These expressions are combined with `;`.
    bodies: Vec<ExpId>,
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

    pub exp_parent: BTreeMap<ExpId, ExpId>,

    pub vars: BTreeMap<VarId, VarDef>,
    pub funs: BTreeMap<FunId, FunDef>,
    pub loops: BTreeMap<LoopId, LoopDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

impl FunDef {
    pub(crate) fn result_ty(&self) -> Option<&Ty> {
        match &self.ty {
            Ty::Fun(tys) => tys.last(),
            _ => None,
        }
    }

    pub(crate) fn bodies(&self) -> Vec<ExpId> {
        self.bodies.clone()
    }
}

impl Sema {
    pub(crate) fn to_doc_msgs(&self) -> Vec<DocMsg> {
        Msg::summarize(self.msgs.values(), &self.syntax).1
    }

    pub(crate) fn is_successful(&self) -> bool {
        self.msgs.iter().all(|(_, msg)| msg.is_successful())
    }

    pub(crate) fn find_module_by_doc_id(&self, doc_id: DocId) -> Option<(ModuleId, &Module)> {
        self.syntax.find_module_by_doc_id(doc_id)
    }

    pub(crate) fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.syntax.exps[&exp_id]
    }

    pub(crate) fn exp_text(&self, exp_id: ExpId) -> &str {
        let exp = self.exp(exp_id);
        let (l, r) = exp.span;
        let module = &self.syntax.modules[&exp.module_id];
        &module.doc().src()[l..r]
    }

    pub(crate) fn symbol_ref(&self, symbol: SymbolKind) -> SymbolRef<'_> {
        match symbol {
            SymbolKind::Prim(prim) => SymbolRef::Prim(prim),
            SymbolKind::Var(var_id) => SymbolRef::Var(var_id, &self.vars[&var_id]),
        }
    }

    pub(crate) fn find_symbol_at(
        &self,
        module_id: ModuleId,
        i: usize,
    ) -> Option<(ExpId, SymbolKind)> {
        let mut exp_id = self.syntax.touch_lowest(module_id, (i, i + 1));

        // Bubble to find corresponding symbol.
        loop {
            if let Some(symbol) = self.exp_symbols.get(&exp_id) {
                return Some((exp_id, symbol.clone()));
            }
            exp_id = *self.exp_parent.get(&exp_id)?;
        }
    }

    /// Find the lowest ancestor let expression of the specified expression.
    pub(crate) fn find_ancestor_let(&self, mut exp_id: ExpId) -> Option<ExpId> {
        loop {
            if let ExpKind::Let { .. } = self.exp(exp_id).kind {
                return Some(exp_id);
            }
            exp_id = *self.exp_parent.get(&exp_id)?;
        }
    }
}

impl BorrowMutMsgs for Sema {
    fn msgs_mut(&mut self) -> &mut Msgs {
        &mut self.msgs
    }
}

pub(crate) const GLOBAL_FUN_ID: FunId = FunId::new(0);
