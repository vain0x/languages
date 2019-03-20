use super::*;
use crate::semantics::{prim::PRIMS, symbol::SymbolKind, ty::Ty, Sema, VarId};
use crate::syntax::keyword::Keyword;
use lsp_types::*;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub(crate) struct CompletionItemData {
    uri: Url,
    var_id: Option<VarId>,
}

impl CompletionItemData {
    pub(crate) fn uri(&self) -> &Url {
        &self.uri
    }
}

#[derive(Serialize, Deserialize)]
struct CompletionItemDataDto {
    uri: String,
    var_id: Option<usize>,
}

impl Serialize for CompletionItemData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        CompletionItemDataDto {
            uri: self.uri.to_string(),
            var_id: self.var_id.map(usize::from),
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for CompletionItemData {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let dto = CompletionItemDataDto::deserialize(deserializer)?;
        Ok(CompletionItemData {
            uri: Url::parse(&dto.uri).unwrap(),
            var_id: dto.var_id.map(VarId::from),
        })
    }
}

pub(crate) fn parse_data(data: serde_json::Value) -> Option<CompletionItemData> {
    serde_json::from_value::<CompletionItemData>(data).ok()
}

fn new_item(
    label: String,
    kind: CompletionItemKind,
    uri: &Url,
    var_id: Option<VarId>,
) -> CompletionItem {
    let data = serde_json::to_value(CompletionItemData {
        uri: uri.clone(),
        var_id: var_id,
    })
    .unwrap();
    CompletionItem {
        label,
        kind: Some(kind),
        data: Some(data),
        ..CompletionItem::default()
    }
}

pub(crate) fn completion(uri: &Url, sema: &Sema, _position: Position) -> CompletionList {
    let mut items = vec![];

    items.extend(Keyword::get_all().into_iter().map(|keyword| {
        new_item(
            keyword.text().to_string(),
            CompletionItemKind::Keyword,
            uri,
            None,
        )
    }));

    items.extend(
        PRIMS
            .iter()
            .map(|(text, _)| new_item(text.to_string(), CompletionItemKind::Constant, uri, None)),
    );

    items.extend(
        Ty::primitive_ty_names()
            .into_iter()
            .map(|name| new_item(name, CompletionItemKind::Struct, uri, None)),
    );

    items.extend(
        sema.all_var_id_names()
            .into_iter()
            .map(|(var_id, name)| new_item(name, CompletionItemKind::Variable, uri, Some(var_id))),
    );

    CompletionList {
        is_incomplete: false,
        items,
    }
}

pub(crate) fn resolve(sema: &Sema, completion_item: &mut CompletionItem, data: CompletionItemData) {
    data.var_id.and_then(|var_id| {
        let text = sema.symbol_definition_text(SymbolKind::Var(var_id))?;
        let value = format!("```{}\n{}\n```", PICOMET_LANG_ID, text);

        completion_item.documentation = Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }));
        Some(())
    });
}

// NOTE: No tests because this feature is still experimental.
