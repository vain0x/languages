//! 名前解決 (name resolution)

use super::*;

fn resolve_var_ident(expr: &mut Expr, symbols: &mut Symbols) {
    match expr.kind() {
        ExprKind::Ident(ident) => {
            let ident = ident.to_owned();
            let var_id = symbols.find_or_new_var(ident.to_owned());
            *expr.kind_mut() = ExprKind::Global(var_id, ident);
        }
        _ => {}
    }
}

fn resolve_fun_ident(expr: &mut Expr, symbols: &mut Symbols) -> usize {
    match expr.kind() {
        ExprKind::Ident(ident) => {
            let ident = ident.to_owned();
            let fun_id = symbols.find_or_new_fun(ident.to_owned());
            *expr.kind_mut() = ExprKind::Fun(fun_id, ident);
            fun_id
        }
        _ => unreachable!("Expected ident for fn"),
    }
}

pub(crate) fn name_res(expr: &mut Expr, symbols: &mut Symbols) {
    match expr.kind() {
        ExprKind::Ident(ident) => match symbols.find(&ident) {
            Some((SymbolKind::Var, var_id)) => {
                *expr.kind_mut() = ExprKind::Global(var_id, ident.to_owned())
            }
            Some((SymbolKind::Fun, fun_id)) => {
                *expr.kind_mut() = ExprKind::Fun(fun_id, ident.to_owned())
            }
            None => panic!("undefined identifier {} at {:?}", ident, expr.main_loc()),
        },
        ExprKind::Assign => match expr.children_mut().as_mut_slice() {
            [left, _right] => {
                resolve_var_ident(left, symbols);
            }
            _ => unreachable!(),
        },
        ExprKind::FunDecl { .. } => match expr.children_mut().as_mut_slice() {
            [ident, _body] => {
                let fun_id = resolve_fun_ident(ident, symbols);
                *expr.kind_mut() = ExprKind::FunDecl { fun_id };
            }
            _ => unreachable!(),
        },
        _ => {}
    }

    for child in expr.children_mut() {
        name_res(child, symbols);
    }
}
