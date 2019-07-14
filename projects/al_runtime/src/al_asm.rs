pub(crate) mod assemble;
pub(crate) mod instr;

pub(crate) use instr::*;

use al_aux::il::*;
use std::collections::HashMap;
use std::mem::replace;

pub(crate) struct Globals(HashMap<String, usize>);

pub(crate) struct AlAsm<'a> {
    pub(crate) il_tree: &'a IlTree,
    pub(crate) globals: Globals,
    pub(crate) instrs: Vec<InstrKind>,
}

impl Globals {
    pub(crate) fn new() -> Globals {
        Globals(HashMap::new())
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn new_global(&mut self, ident: String) {
        let global_id = self.len();
        self.0.insert(ident, global_id);
    }

    pub(crate) fn find(&self, ident: &str) -> Option<usize> {
        self.0.get(ident).cloned()
    }
}

impl AlAsm<'_> {
    pub(crate) fn new(il_tree: &IlTree) -> AlAsm<'_> {
        AlAsm {
            il_tree,
            globals: Globals::new(),
            instrs: vec![],
        }
    }

    pub(crate) fn il(&self) -> &IlTree {
        self.il_tree
    }

    pub(crate) fn new_instr(&mut self, instr: InstrKind) {
        self.instrs.push(instr);
    }

    pub(crate) fn take_instrs(&mut self) -> Vec<InstrKind> {
        replace(&mut self.instrs, vec![])
    }

    pub(crate) fn global_count(&self) -> usize {
        self.globals.len()
    }
}
