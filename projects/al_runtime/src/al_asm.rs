pub(crate) mod assemble;
pub(crate) mod ins;

pub(crate) use ins::*;

use std::collections::HashMap;

pub(crate) struct Globals(HashMap<String, usize>);

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

    pub(crate) fn find(&mut self, ident: &str) -> Option<usize> {
        self.0.get(ident).cloned()
    }
}
