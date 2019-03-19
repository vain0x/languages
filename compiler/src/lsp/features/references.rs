use crate::semantics::Sema;
use crate::syntax::DocId;
use lsp_types::*;
use std::cmp::min;

pub(crate) fn do_references(
    doc_id: DocId,
    sema: &Sema,
    position: Position,
    include_definition: bool,
) -> Option<Vec<crate::syntax::Range>> {
    let line = position.line as usize;
    let column = position.character as usize;

    let (module_id, module) = sema.find_module_by_doc_id(doc_id)?;
    let i = module.doc().unlocate(line, column);

    let (_, symbol_kind) = sema
        .find_symbol_at(module_id, i)
        .or_else(|| sema.find_symbol_at(module_id, i - min(i, 1)))?;
    let def_exp_id = sema.symbol_ref(symbol_kind).def_exp_id();

    let locations = sema
        .find_symbol_occurrances(module_id, symbol_kind)
        .into_iter()
        .filter_map(|exp_id| {
            if !include_definition && def_exp_id.map(|x| x == exp_id).unwrap_or(false) {
                None
            } else {
                Some(sema.syntax.locate_exp(exp_id))
            }
        })
        .collect();
    Some(locations)
}

// FIXME: This doesn't find references in other documents.
pub(crate) fn references(
    doc_id: DocId,
    sema: &Sema,
    uri: &Url,
    position: Position,
    include_definition: bool,
) -> Vec<Location> {
    do_references(doc_id, sema, position, include_definition)
        .unwrap_or(vec![])
        .into_iter()
        .map(|((ly, lx), (ry, rx))| {
            Location::new(
                uri.clone(),
                Range::new(
                    Position::new(ly as u64, lx as u64),
                    Position::new(ry as u64, rx as u64),
                ),
            )
        })
        .collect()
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
            println_int(num + num);

            let num = 2;
        "#;
        let doc_id = DocId::new(0);
        let sema = analyze::analyze_str(SRC);

        // On `num` in first let.
        let references = do_references(doc_id, &sema, Position::new(1, 16), true).unwrap();
        assert_eq!(references.len(), 3);
        assert_eq!(references[0], ((1, 16), (1, 19)));
        assert_eq!(references[1], ((2, 24), (2, 27)));
        assert_eq!(references[2], ((2, 30), (2, 33)));

        // On `num` in call.
        let references = do_references(doc_id, &sema, Position::new(2, 24), true).unwrap();
        assert_eq!(references.len(), 3);

        // On `num` without defintion.
        let references = do_references(doc_id, &sema, Position::new(1, 16), false).unwrap();
        assert_eq!(references.len(), 2);
    }
}
