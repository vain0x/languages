use crate::*;

pub struct Sema {
    pub syntax: Rc<Syntax>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Prim {
    Add,
}

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolKind {
    Var,
    Fun,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Symbol {
    kind: SymbolKind,
    name: String,
}

pub struct SemanticAnalyzer {
    sema: Sema,
    current_fun_id: FunId,
}

impl SemanticAnalyzer {
    fn sema(&mut self) {}
}

pub fn sema(syntax: Rc<Syntax>) -> Sema {
    let mut analyzer = SemanticAnalyzer {
        sema: Sema {
            syntax: Rc::clone(&syntax),
            msgs: syntax.msgs.clone(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    analyzer.sema();
    analyzer.sema
}
