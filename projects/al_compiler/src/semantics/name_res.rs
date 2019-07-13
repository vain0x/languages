//! 名前解決 (name resolution)

use super::*;
use std::collections::HashMap;

pub(crate) fn name_res(expr: &mut Expr, globals: &mut HashMap<String, usize>) {
    match expr.kind() {
        ExprKind::Ident(ident) => {
            let ident = ident.to_owned();
            let global_id = match globals.get(&ident) {
                Some(global_id) => *global_id,
                None => {
                    let global_id = globals.len();
                    globals.insert(ident.to_owned(), global_id);
                    global_id
                }
            };
            *expr.kind_mut() = ExprKind::Global(global_id, ident);
        }
        _ => {}
    }

    for child in expr.children_mut() {
        name_res(child, globals);
    }
}
