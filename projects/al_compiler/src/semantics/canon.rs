//! 正準化 (canonicalize)
//!
//! 式を項と文に分類する。
//!
//! semi 文の直下はすべて文にする。そのため、semi 文の直下にある項は do 文に変形する。

use super::*;
use std::mem::replace;

/// 式を項とみなして正準化する。
/// 副作用を持つ入れ子の式は stmts に移動する。
fn canon_term(expr: &mut Expr, stmts: &mut Vec<Expr>) {
    for child in expr.children_mut() {
        canon_term(child, stmts);
    }

    match expr.kind() {
        ExprKind::Lit(..) | ExprKind::Prim(..) | ExprKind::Ident(..) | ExprKind::Global(..) => {}
        ExprKind::Call => {}
        ExprKind::Assign => unimplemented!("canon assign"),
        ExprKind::Do => unimplemented!("canon do"),
        ExprKind::Semi => unimplemented!("canon semi"),
    }
}

/// 式を文とみなして正準化する。
fn canon_stmt(mut expr: Expr, stmts: &mut Vec<Expr>) {
    match expr.kind() {
        ExprKind::Lit(..)
        | ExprKind::Prim(..)
        | ExprKind::Ident(..)
        | ExprKind::Global(..)
        | ExprKind::Call => {
            canon_term(&mut expr, stmts);
            stmts.push(expr.into_do());
        }
        ExprKind::Assign | ExprKind::Do => {
            // NOTE: Assign の左辺はパターン
            for child in expr.children_mut() {
                canon_term(child, stmts);
            }
            stmts.push(expr);
        }
        ExprKind::Semi => {
            for child in expr.children_mut().drain(..) {
                canon_stmt(child, stmts)
            }
        }
    }
}

pub(crate) fn canon(expr: &mut Expr) {
    let (main_loc, total_loc) = (*expr.main_loc(), *expr.total_loc());
    let semi = Expr::new(ExprKind::Semi, vec![], main_loc, total_loc);

    let mut stmts = vec![];
    canon_stmt(replace(expr, semi), &mut stmts);

    expr.children_mut().extend(stmts);
}
