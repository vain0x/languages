use crate::semantics::Sema;
use crate::syntax::DocId;
use lsp_types::*;
use std::cmp::min;

pub(crate) fn do_rename(
    doc_id: DocId,
    sema: &Sema,
    position: Position,
) -> Option<Vec<crate::syntax::Range>> {
    let line = position.line as usize;
    let column = position.character as usize;

    let (module_id, module) = sema.find_module_by_doc_id(doc_id)?;
    let i = module.doc().unlocate(line, column);

    let (_, symbol_kind) = sema
        .find_symbol_at(module_id, i)
        .or_else(|| sema.find_symbol_at(module_id, i - min(i, 1)))?;
    if !symbol_kind.is_var() {
        return None;
    }

    let ranges = sema
        .find_symbol_occurrences(module_id, symbol_kind)
        .into_iter()
        .map(|exp_id| sema.syntax.locate_exp(exp_id))
        .collect();
    Some(ranges)
}

// FIXME: This doesn't rename occurrences in other documents.
pub(crate) fn rename(
    doc_id: DocId,
    sema: &Sema,
    uri: &Url,
    doc_version: u64,
    position: Position,
    new_name: String,
) -> Option<WorkspaceEdit> {
    let ranges = do_rename(doc_id, sema, position).unwrap_or(vec![]);
    if ranges.is_empty() {
        return None;
    }

    let edits = ranges
        .into_iter()
        .map(|((ly, lx), (ry, rx))| {
            TextEdit::new(
                Range::new(
                    Position::new(ly as u64, lx as u64),
                    Position::new(ry as u64, rx as u64),
                ),
                new_name.to_string(),
            )
        })
        .collect();
    let text_document_edits = vec![TextDocumentEdit {
        text_document: VersionedTextDocumentIdentifier::new(uri.clone(), doc_version),
        edits,
    }];
    let workspace_edit = WorkspaceEdit {
        changes: None,
        document_changes: Some(DocumentChanges::Edits(text_document_edits)),
    };
    Some(workspace_edit)
}

// NOTE: No tests because testing `references` almost covers this.
// FIXME: Add 'it can't rename primitive symbol' case?
