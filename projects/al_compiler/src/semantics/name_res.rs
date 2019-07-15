//! 名前解決 (name resolution)

use super::*;

pub(crate) fn name_res(expr: &mut Expr, symbols: &mut Symbols) {
    match expr.kind() {
        ExprKind::Ident(ident) => {
            let ident = ident.to_owned();
            let var_id = symbols.find_or_new_var(ident.to_owned());
            *expr.kind_mut() = ExprKind::Global(var_id, ident);
        }
        _ => {}
    }

    for child in expr.children_mut() {
        name_res(child, symbols);
    }
}
