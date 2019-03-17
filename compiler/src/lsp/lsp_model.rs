use crate::syntax::DocId;
use crate::lsp::features;
use crate::semantics::{analyze, Sema};
use lsp_types::*;
use std::rc::Rc;
use std::collections::BTreeMap;

pub(super) struct DocAnalysis {
    doc_id: DocId,
    sema: Sema,
}

pub(super) struct LspModel {
    next_doc_id: DocId,
    docs: BTreeMap<Url, DocAnalysis>,
}

impl LspModel {
    pub(super) fn new() -> Self {
        LspModel {
            docs: BTreeMap::new(),
            next_doc_id: DocId::new(0),
        }
    }

    fn fresh_doc_id(&mut self) -> DocId {
        self.next_doc_id += 1;
        self.next_doc_id
    }

    pub(super) fn open_doc(&mut self, uri: Url, text: String) {
        let doc_id = self.fresh_doc_id();
        let sema = analyze::analyze_str(doc_id, Rc::new(text));
        self.docs.insert(uri, DocAnalysis { doc_id, sema });
    }

    pub(super) fn change_doc(&mut self, uri: Url, text: String) {
        self.open_doc(uri, text);
    }

    pub(super) fn validate(&mut self, uri: &Url) -> Vec<Diagnostic> {
        let analysis = match self.docs.get(uri) {
            None => {
                debug!("Doc {} is not compiled yet.", uri);
                return vec![];
            }
            Some(analysis) => analysis,
        };

        features::diagnostics::sema_to_diagnostics(&analysis.sema)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn main_uri() -> Url {
        Url::parse("file:///src/main.picomet").unwrap()
    }

    fn make_range(ly: u64, lx: u64, ry: u64, rx: u64) -> Range {
        Range::new(Position::new(ly, lx), Position::new(ry, rx))
    }

    #[test]
    fn test_validate() {
        let mut model = LspModel::new();
        model.open_doc(main_uri(), "0 + ()".to_string());

        let diagnostics = model.validate(&main_uri());
        assert_eq!(diagnostics.len(), 1);

        assert_eq!(
            diagnostics[0],
            Diagnostic {
                severity: Some(DiagnosticSeverity::Error),
                message: "Type Error".to_string(),
                range: make_range(0, 4, 0, 6),
                source: Some("Picomet-lang LSP".to_string()),
                ..Diagnostic::default()
            }
        );
    }
}
