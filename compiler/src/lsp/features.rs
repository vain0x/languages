use lsp_types::*;

pub(super) fn validate_document(src: &str) -> Vec<Diagnostic> {
    let result = crate::compile(src);
    let mut diagnostics = vec![];
    for msg in &result.msgs {
        diagnostics.push(Diagnostic {
            range: Range {
                start: Position {
                    line: 1,
                    character: 1,
                },
                end: Position {
                    line: 1,
                    character: 2,
                },
            },
            message: msg.message().to_string(),
            source: Some("Picomet-lang".to_string()),
            ..Diagnostic::default()
        })
    }
    diagnostics
}
