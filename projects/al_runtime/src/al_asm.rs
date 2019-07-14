pub(crate) mod assemble;
pub(crate) mod instr;

pub(crate) use instr::*;

use std::collections::HashMap;

pub(crate) struct Globals(HashMap<String, usize>);

pub(crate) struct AlAsm {
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

impl AlAsm {
    pub(crate) fn new() -> AlAsm {
        AlAsm {
            globals: Globals::new(),
            instrs: vec![],
        }
    }

    pub(crate) fn new_instr(&mut self, instr: InstrKind) {
        self.instrs.push(instr);
    }

    pub(crate) fn global_count(&self) -> usize {
        self.globals.len()
    }
}
