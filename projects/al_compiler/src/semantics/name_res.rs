//! 名前解決 (name resolution)

use super::*;

fn resolve_fun_ident(expr: &mut Expr, symbols: &mut Symbols) {
    match expr.kind() {
        ExprKind::Ident(ident) => {
            let ident = ident.to_owned();
            let var_id = symbols.find_or_new_fun(ident.to_owned());
            *expr.kind_mut() = ExprKind::Fun(var_id, ident);
        }
        _ => unreachable!("Expected ident for fn"),
    }
}

pub(crate) fn name_res(expr: &mut Expr, symbols: &mut Symbols) {
    match expr.kind() {
        ExprKind::Ident(ident) => {
            let ident = ident.to_owned();
            let var_id = symbols.find_or_new_var(ident.to_owned());
            *expr.kind_mut() = ExprKind::Global(var_id, ident);
        }
        ExprKind::FunDecl => match expr.children_mut().as_mut_slice() {
            [ident, _body] => {
                resolve_fun_ident(ident, symbols);
            }
            _ => {}
        },
        _ => {}
    }

    for child in expr.children_mut() {
        name_res(child, symbols);
    }
}
