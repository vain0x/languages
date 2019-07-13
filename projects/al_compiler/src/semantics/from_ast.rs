use super::*;
use crate::syntax::*;

fn bin_op_to_prim(bin_op: BinOp) -> Prim {
    match bin_op {
        BinOp::Add => Prim::OpAdd,
        BinOp::Sub => Prim::OpSub,
        BinOp::Mul => Prim::OpMul,
        BinOp::Div => Prim::OpDiv,
        BinOp::Eq => Prim::OpEq,
    }
}

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
        AstKind::BinOp(bin_op) => {
            let prim = bin_op_to_prim(*bin_op);

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
