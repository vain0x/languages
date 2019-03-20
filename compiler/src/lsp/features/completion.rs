use crate::semantics::{prim::PRIMS, ty::Ty, Sema};
use crate::syntax::keyword::Keyword;
use lsp_types::*;

fn new_item(label: String, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label,
        kind: Some(kind),
        ..CompletionItem::default()
    }
}

pub(crate) fn completion(sema: &Sema, _position: Position) -> CompletionList {
    let mut items = vec![];

    items.extend(
        Keyword::get_all()
            .into_iter()
            .map(|keyword| new_item(keyword.text().to_string(), CompletionItemKind::Keyword)),
    );

    items.extend(
        PRIMS
            .iter()
            .map(|(text, _)| new_item(text.to_string(), CompletionItemKind::Constant)),
    );

    items.extend(
        Ty::primitive_ty_names()
            .into_iter()
            .map(|name| new_item(name, CompletionItemKind::Struct)),
    );

    items.extend(
        sema.all_var_names()
            .into_iter()
            .map(|name| new_item(name, CompletionItemKind::Variable)),
    );

    CompletionList {
        is_incomplete: false,
        items,
    }
}

// NOTE: No tests because this feature is still experimental.
