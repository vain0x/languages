use crate::semantics::Sema;
use crate::syntax::DocId;
use crate::syntax::ExpKind;
use lsp_types::*;

pub(crate) fn hover(doc_id: DocId, sema: &Sema, position: Position) -> Option<Hover> {
    let (module_id, module) = sema.find_module_by_doc_id(doc_id)?;

    let i = module
        .doc()
        .unlocate(position.line as usize, position.character as usize);

    for (&exp_id, symbol) in &sema.exp_symbols {
        let exp = &sema.syntax.exps[&exp_id];
        if exp.module_id != module_id {
            continue;
        }

        match exp.kind {
            ExpKind::Ident(_) => {}
            _ => continue,
        }

        let (l, r) = exp.span;
        if l <= i && i <= r {
            let ty = sema
                .exp_tys
                .get(&exp_id)
                .map(|ty| format!("{:?}", ty))
                .unwrap_or("{unknown}".to_string());
            return Some(Hover {
                contents: HoverContents::Array(vec![
                    MarkedString::String(ty),
                    MarkedString::String(format!("{:?}", symbol)),
                ]),
                range: None,
            });
        }
    }
    None
}
