pub(super) mod completion;
pub(super) mod diagnostics;
pub(super) mod hover;
pub(super) mod references;
pub(super) mod rename;

use lsp_types::*;

const PICOMET_LANG_ID: &str = "picomet-lang";

fn marked_str(value: String) -> MarkedString {
    MarkedString::LanguageString(LanguageString {
        language: PICOMET_LANG_ID.to_string(),
        value,
    })
}
