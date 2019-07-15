//! 正準化 (canonicalize)
//!
//! 式を項と文と宣言に分類する。
//!
//! semi 文の直下をすべて文になるように変形する。
//! 例えば、semi 文の直下にある項 (式文) は do 文に変形する。

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
        ExprKind::If => unimplemented!("canon if"),
        ExprKind::FnDecl => unimplemented!("canon fn"),
        ExprKind::Semi => unimplemented!("canon semi"),
    }
}

/// 式を文とみなして正準化する。
fn canon_stmt(mut expr: Expr, stmts: &mut Vec<Expr>, decls: &mut Vec<Expr>) {
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
        ExprKind::If => {
            match expr.children_mut().as_mut_slice() {
                [cond, body, alt] => {
                    canon_term(cond, stmts);
                    canon_block(body, decls);
                    canon_block(alt, decls);
                }
                _ => unimplemented!("no else yet"),
            }
            stmts.push(expr);
        }
        ExprKind::FnDecl => {
            decls.push(expr);
        }
        ExprKind::Semi => {
            for child in expr.children_mut().drain(..) {
                canon_stmt(child, stmts, decls)
            }
        }
    }
}

/// 式をブロックとみなして正準化する。
/// 式の内部に含まれる複数の文からなる1個の semi 文に式を変換する。
/// 文の子要素はすべて文であり、文の孫要素は文または項であり、項の子孫要素はすべて項である。
fn canon_block(expr: &mut Expr, decls: &mut Vec<Expr>) {
    let (main_loc, total_loc) = (*expr.main_loc(), *expr.total_loc());
    let semi = Expr::new(ExprKind::Semi, vec![], main_loc, total_loc);

    let mut stmts = vec![];
    canon_stmt(replace(expr, semi), &mut stmts, decls);

    expr.children_mut().extend(stmts);

    // 1要素の semi 文を unwrap する。
    if expr.children().len() == 1 {
        let inner = expr.children_mut().drain(..).next().unwrap();
        *expr = inner;
    }
}

/// 式を宣言とみなして正準化する。
/// トップレベルは semi 宣言であり、直下に宣言または文が並ぶ。
fn canon_decl(expr: &mut Expr) {
    let (main_loc, total_loc) = (*expr.main_loc(), *expr.total_loc());
    let mut semi = Expr::new(ExprKind::Semi, vec![], main_loc, total_loc);

    let mut decls = vec![];
    canon_block(expr, &mut decls);

    semi.children_mut().extend(decls);

    let last = replace(expr, semi);
    expr.children_mut().push(last);

    // 1要素の semi 文を unwrap する。
    if expr.children().len() == 1 {
        let inner = expr.children_mut().drain(..).next().unwrap();
        *expr = inner;
    }
}

pub(crate) fn canon(expr: &mut Expr) {
    canon_decl(expr);
}
