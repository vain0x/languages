use crate::semantics::Sema;
use crate::syntax::DocId;
use lsp_types::*;

const PICOMET_LANG_ID: &str = "picomet-lang";

fn marked_str(value: String) -> MarkedString {
    MarkedString::LanguageString(LanguageString {
        language: PICOMET_LANG_ID.to_string(),
        value,
    })
}

pub(crate) fn do_hover(doc_id: DocId, sema: &Sema, position: Position) -> Option<Vec<String>> {
    let line = position.line as usize;
    let column = position.character as usize;
    let mut contents = vec![];

    let (module_id, module) = sema.find_module_by_doc_id(doc_id)?;

    // Find type.
    let i = module.doc().unlocate(line, column);
    let exp_id = sema.syntax.touch_lowest(module_id, (i, i + 1));
    let token_id = sema.syntax.touch_token(module_id, (i, i + 1));

    if token_id.is_some() && !sema.exp(exp_id).kind.is_stmt() {
        let ty = format!("{}", sema.get_ty(exp_id));
        contents.push(ty);
    }

    // Find definition expression text.
    // FIXME: This may be unnecessary becauase VSCode provide `peek defintion`.
    let def_text = sema
        .find_symbol_at(module_id, i)
        .and_then(|(_, symbol)| sema.symbol_ref(symbol).def_exp_id())
        .and_then(|exp_id| sema.find_ancestor_let(exp_id))
        .map(|let_exp_id| sema.exp_text(let_exp_id));
    if let Some(text) = def_text {
        contents.push(text.to_string());
    }

    if contents.is_empty() {
        return None;
    }
    Some(contents)
}

pub(crate) fn hover(doc_id: DocId, sema: &Sema, position: Position) -> Option<Hover> {
    let contents = do_hover(doc_id, sema, position)?;
    let hover = Hover {
        contents: HoverContents::Array(contents.into_iter().map(marked_str).collect()),
        range: None,
    };
    Some(hover)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantics::analyze;
    use crate::syntax::DocId;

    #[test]
    fn test_hover() {
        const SRC: &str = r#"
            let num = 1;
            println_int(num + 1);
        "#;
        let doc_id = DocId::new(0);
        let sema = analyze::analyze_str(SRC);

        // On `num`.
        let hover = do_hover(doc_id, &sema, Position::new(1, 16)).expect("Some(Hover)");
        assert_eq!(hover[0], "int");
        assert_eq!(hover[1], "let num = 1");

        // On nothing.
        let hover = do_hover(doc_id, &sema, Position::new(0, 0));
        assert!(hover.is_none());

        // On `+`.
        let hover = do_hover(doc_id, &sema, Position::new(2, 28)).expect("Some(Hover)");
        assert_eq!(hover[0], "int");

        // On space.
        let hover = do_hover(doc_id, &sema, Position::new(2, 29));
        assert!(hover.is_none());
    }
}
