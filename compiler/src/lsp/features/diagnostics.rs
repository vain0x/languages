use crate::semantics::{DocMsg, Sema};
use lsp_types::*;

const DIAGNOSTIC_SOURCE: &str = "Picomet-lang LSP";

fn msg_to_diagnostic(msg: &DocMsg) -> Diagnostic {
    let (ly, lx) = msg.start_pos();
    let (ry, rx) = msg.end_pos();
    Diagnostic {
        severity: Some(DiagnosticSeverity::Error),
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
        source: Some(DIAGNOSTIC_SOURCE.to_string()),
        ..Diagnostic::default()
    }
}

pub(crate) fn sema_to_diagnostics(sema: &Sema) -> Vec<Diagnostic> {
    let msgs = sema.to_doc_msgs();
    let mut diagnostics = vec![];
    for msg in &msgs {
        diagnostics.push(msg_to_diagnostic(msg))
    }
    diagnostics
}
