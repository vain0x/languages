use crate::DocMsg;
use lsp_types::*;

fn msg_to_diagnostic(msg: &DocMsg) -> Diagnostic {
    let (ly, lx) = msg.start_pos();
    let (ry, rx) = msg.end_pos();
    Diagnostic {
        range: Range {
            start: Position {
                line: ly as u64,
                character: lx as u64,
            },
            end: Position {
                line: ry as u64,
                character: rx as u64,
            },
        },
        message: msg.message().to_string(),
        source: Some("Picomet-lang".to_string()),
        ..Diagnostic::default()
    }
}

pub(super) fn validate_document(src: &str) -> Vec<Diagnostic> {
    let result = crate::compile(src);
    let mut diagnostics = vec![];
    for msg in &result.msgs {
        diagnostics.push(msg_to_diagnostic(msg))
    }
    diagnostics
}
