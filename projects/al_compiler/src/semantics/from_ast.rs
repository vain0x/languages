use super::*;
use crate::syntax::*;

fn map_children<'a>(ast: &'a Ast) -> impl Iterator<Item = Expr> + 'a {
    ast.children().iter().map(from_ast)
}

pub(crate) fn from_ast(ast: &Ast) -> Expr {
    match ast.kind() {
        AstKind::True => Expr::new_bool(true, ast.loc()),
        AstKind::False => Expr::new_bool(false, ast.loc()),
        AstKind::Ident(ident) => Expr::new_ident(ident.to_owned(), ast.loc()),
        AstKind::Int(value) => Expr::new_int(*value, ast.loc()),
        AstKind::Assert => Expr::new_prim(Prim::Assert, ast.loc()),
        AstKind::Add | AstKind::Sub | AstKind::Mul | AstKind::Div |
        AstKind::Eq => {
            let prim = match ast.kind() {
                AstKind::Add => Prim::OpAdd,
                AstKind::Sub => Prim::OpSub,
                AstKind::Mul => Prim::OpMul,
                AstKind::Div => Prim::OpDiv,
                AstKind::Eq => Prim::OpEq,
                _ => unreachable!()
            };
            let mut children = vec![];
            children.push(Expr::new_prim(prim, ast.loc()));
            children.extend(map_children(ast));
            Expr::new(ExprKind::Call, children, ast.loc(), ast.total_loc())
        }
        AstKind::Call => {
            Expr::new(ExprKind::Call, map_children(ast).collect(), ast.loc(), ast.total_loc())
        }
        AstKind::Semi => {
            Expr::new(ExprKind::Semi, map_children(ast).collect(), ast.loc(), ast.total_loc())
        }
    }
}
