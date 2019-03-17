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
    let (exp_id, symbol) = sema.find_symbol_at(module_id, i)?;

    let ty = format!("{}", sema.get_ty(exp_id));
    contents.push(ty);

    // Find definition expression text.
    let symbol = sema.symbol_ref(symbol);
    let def_text = symbol
        .def_exp_id()
        .and_then(|def_exp_id| sema.find_ancestor_let(def_exp_id))
        .map(|let_exp_id| sema.exp_text(let_exp_id));
    if let Some(text) = def_text {
        contents.push(text.to_string());
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
            println_int(num);
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
    }
}
