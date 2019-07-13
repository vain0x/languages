use crate::semantics::*;
use crate::syntax::*;
use al_aux::il::*;

fn kind_from_prim(prim: Prim) -> IlKind {
    match prim {
        Prim::Assert => IlKind::Assert,
        Prim::OpAdd => IlKind::OpAdd,
        Prim::OpSub => IlKind::OpSub,
        Prim::OpMul => IlKind::OpMul,
        Prim::OpDiv => IlKind::OpDiv,
        Prim::OpEq => IlKind::OpEq,
    }
}

fn set_text_comment(il: usize, expr: &Expr, t: &mut IlTree, s: &SourceFileSystem) {
    let (text, omit) = expr.short_text(s);
    let comment = format!("{}{}", text, if omit { ".." } else { "" });
    t.set_comment(il, comment);
}

fn gen_expr(expr: &Expr, t: &mut IlTree, s: &SourceFileSystem) -> usize {
    match expr.kind() {
        ExprKind::Lit(Lit::Bool(value)) => t.add_bool(*value),
        ExprKind::Lit(Lit::Int(value)) => t.add_int(*value),
        ExprKind::Prim(prim) => t.add_leaf(kind_from_prim(*prim)),
        ExprKind::Ident(_) => unimplemented!(),
        ExprKind::Call => {
            let kind = match expr.children()[0].kind() {
                ExprKind::Prim(prim) => kind_from_prim(*prim),
                _ => unimplemented!(),
            };
            let mut children = vec![];
            for child in &expr.children()[1..] {
                children.push(gen_expr(child, t, s));
            }
            let il = t.add_node(kind, &children);
            if expr.is_single_statement() {
                set_text_comment(il, expr, t, s);
            }
            il
        }
        ExprKind::Semi => {
            let mut children = vec![];
            for child in expr.children() {
                children.push(gen_expr(child, t, s));
            }
            t.add_node(IlKind::Semi, &children)
        }
    }
}

pub(crate) fn from_expr(expr: &Expr, s: &SourceFileSystem) -> IlTree {
    let mut il_tree = IlTree::new();

    let top_level = gen_expr(expr, &mut il_tree, s);
    let code_section = il_tree.add_node(IlKind::CodeSection, &[top_level]);
    let root = il_tree.add_node(IlKind::Root, &[code_section]);
    il_tree.set_root(root);

    il_tree
}
