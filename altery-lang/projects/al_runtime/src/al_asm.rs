pub(crate) mod assemble;
pub(crate) mod instr;

pub(crate) use instr::*;

use std::collections::HashMap;

pub(crate) struct Globals(HashMap<String, usize>);

struct Label {
    name: String,
    def_site: Option<usize>,
    use_sites: Vec<usize>,
}

pub(crate) struct Labels {
    inner: Vec<Label>,
}

pub(crate) struct AlAsm {
    pub(crate) globals: Globals,
    pub(crate) labels: Labels,
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

impl Label {
    fn new(name: String) -> Label {
        Label {
            name,
            def_site: None,
            use_sites: vec![],
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn def_site(&self) -> Option<usize> {
        self.def_site
    }

    fn use_sites(&self) -> &[usize] {
        &self.use_sites
    }

    fn set_def_site(&mut self, def_site: usize) {
        self.def_site = Some(def_site);
    }

    fn add_use_site(&mut self, use_site: usize) {
        self.use_sites.push(use_site);
    }
}

impl Labels {
    fn new() -> Labels {
        Labels { inner: vec![] }
    }

    fn new_label(&mut self, name: String) -> usize {
        let label = self.inner.len();
        self.inner.push(Label::new(name));
        label
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn find(&self, name: &str) -> Option<usize> {
        self.inner.iter().position(|label| label.name() == name)
    }

    fn def_site(&self, label: usize) -> Option<usize> {
        self.inner[label].def_site()
    }

    fn use_sites(&self, label: usize) -> &[usize] {
        self.inner[label].use_sites()
    }

    fn def_label(&mut self, label: usize, def_site: usize) {
        self.inner[label].set_def_site(def_site);
    }

    fn use_label(&mut self, label: usize, use_site: usize) {
        self.inner[label].add_use_site(use_site);
    }
}

impl AlAsm {
    pub(crate) fn new() -> AlAsm {
        AlAsm {
            globals: Globals::new(),
            labels: Labels::new(),
            instrs: vec![],
        }
    }

    pub(crate) fn new_instr(&mut self, instr: InstrKind) {
        self.instrs.push(instr);
    }

    pub(crate) fn global_count(&self) -> usize {
        self.globals.len()
    }

    pub(crate) fn resolve_labels(&mut self) {
        for label in 0..self.labels.len() {
            let def_site = match self.labels.def_site(label) {
                None => unreachable!("ラベルは配置済みのはず"),
                Some(label) => label,
            };

            for &use_site in self.labels.use_sites(label) {
                match self.instrs[use_site] {
                    InstrKind::Label(l) if l == label => {
                        self.instrs[use_site] = InstrKind::Pc(def_site);
                    }
                    _ => unreachable!("ラベルの使用箇所の命令はラベルのはず"),
                }
            }
        }
    }
}
