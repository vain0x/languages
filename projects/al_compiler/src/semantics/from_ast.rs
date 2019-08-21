//! 抽象構文木から HIR への変換

use super::*;
use crate::syntax::*;

fn bin_op_to_prim(bin_op: BinOp) -> Prim {
    match bin_op {
        BinOp::Add => Prim::OpAdd,
        BinOp::Sub => Prim::OpSub,
        BinOp::Mul => Prim::OpMul,
        BinOp::Div => Prim::OpDiv,
        BinOp::Eq => Prim::OpEq,
        BinOp::Assign => panic!(),
    }
}

fn map_children<'a>(ast: &'a Ast) -> impl Iterator<Item = Expr> + 'a {
    ast.children().iter().map(from_ast)
}

pub(crate) fn from_ast(ast: &Ast) -> Expr {
    match ast.kind() {
        AstKind::True => Expr::new_bool(true, *ast.extent()),
        AstKind::False => Expr::new_bool(false, *ast.extent()),
        AstKind::Ident(ident) => Expr::new_ident(ident.to_owned(), *ast.extent()),
        AstKind::Int(value) => Expr::new_int(*value, *ast.extent()),
        AstKind::Assert => Expr::new_prim(Prim::Assert, *ast.extent()),
        AstKind::If => Expr::new(
            ExprKind::If,
            map_children(ast).collect(),
            *ast.extent(),
        ),
        AstKind::FunDecl => Expr::new(
            ExprKind::FunDecl { fun_id: 0 },
            map_children(ast).collect(),
            *ast.extent(),
        ),
        AstKind::Bin(BinOp::Assign) => Expr::new(
            ExprKind::Assign,
            map_children(ast).collect(),
            *ast.extent(),
        ),
        AstKind::Bin(bin_op) => {
            let prim = bin_op_to_prim(*bin_op);

            let mut children = vec![];
            children.push(Expr::new_prim(prim, *ast.extent()));
            children.extend(map_children(ast));

            Expr::new(ExprKind::Call, children, *ast.extent())
        }
        AstKind::Call => Expr::new(
            ExprKind::Call,
            map_children(ast).collect(),
            *ast.extent(),
        ),
        AstKind::Semi => Expr::new(
            ExprKind::Semi,
            map_children(ast).collect(),
            *ast.extent(),
        ),
    }
}
