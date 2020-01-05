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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantics::analyze;

    fn make_range(ly: u64, lx: u64, ry: u64, rx: u64) -> Range {
        Range::new(Position::new(ly, lx), Position::new(ry, rx))
    }

    #[test]
    fn test_validate() {
        let sema = analyze::analyze_str("0 + ()");

        let diagnostics = sema_to_diagnostics(&sema);

        assert_eq!(diagnostics.len(), 1, "diagnostics = {:?}", diagnostics);
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
