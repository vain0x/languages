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

fn gen_globals(t: &mut IlTree) -> usize {
    let mut children = vec![];
    for &ident in &["a", "b"] {
        children.push(t.new_ident(ident.to_owned()));
    }
    t.new_node(IlKind::Globals, &children)
}

fn gen_expr(expr: &Expr, t: &mut IlTree, s: &SourceFileSystem) -> usize {
    let il = match expr.kind() {
        ExprKind::Lit(Lit::Bool(value)) => t.new_bool(*value),
        ExprKind::Lit(Lit::Int(value)) => t.new_int(*value),
        ExprKind::Prim(prim) => t.new_leaf(kind_from_prim(*prim)),
        ExprKind::Ident(name) => {
            let ident = t.new_ident(name.to_owned());
            t.new_node(IlKind::GlobalGet, &[ident])
        }
        ExprKind::Assign => match expr.children() {
            [left, right] => {
                let left = gen_expr(left, t, s);
                let right = gen_expr(right, t, s);
                t.new_node(IlKind::CellSet, &[left, right])
            }
            _ => unreachable!(),
        },
        ExprKind::Call => {
            let kind = match expr.children()[0].kind() {
                ExprKind::Prim(prim) => kind_from_prim(*prim),
                _ => unimplemented!(),
            };
            let mut children = vec![];
            for child in &expr.children()[1..] {
                children.push(gen_expr(child, t, s));
            }
            t.new_node(kind, &children)
        }
        ExprKind::Semi => {
            let mut children = vec![];
            for child in expr.children() {
                children.push(gen_expr(child, t, s));
            }
            t.new_node(IlKind::Semi, &children)
        }
    };

    if expr.is_single_statement() {
        set_text_comment(il, expr, t, s);
    }

    il
}

pub(crate) fn from_expr(expr: &Expr, s: &SourceFileSystem) -> IlTree {
    let mut il_tree = IlTree::new();

    let top_level = gen_expr(expr, &mut il_tree, s);
    let globals = gen_globals(&mut il_tree);
    let code_section = il_tree.new_node(IlKind::CodeSection, &[top_level]);
    let root = il_tree.new_node(IlKind::Root, &[globals, code_section]);
    il_tree.set_root(root);

    il_tree
}
