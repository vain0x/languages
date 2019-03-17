use lsp_types::*;

pub(crate) fn hover(position: Position) -> Option<Hover> {
    if position.line % 2 == 0 {
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(format!(
                "{}:{}",
                position.line, position.character
            ))),
            range: None,
        })
    } else {
        None
    }
}
