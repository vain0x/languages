//! HIR から中間言語への変換

use crate::semantics::*;
use crate::syntax::*;
use al_aux::il::*;

#[derive(Clone, Copy)]
struct Label {
    ident_il: usize,
}

impl Label {
    fn new(ident_il: usize) -> Label {
        Label { ident_il }
    }

    fn new_def_node(&self, t: &mut IlTree) -> usize {
        t.new_node(IlKind::LabelDef, &[self.ident_il])
    }

    fn new_get_node(&self, t: &mut IlTree) -> usize {
        t.new_node(IlKind::LabelGet, &[self.ident_il])
    }

    fn new_jump_node(&self, t: &mut IlTree) -> usize {
        let label = self.new_get_node(t);
        t.new_node(IlKind::Jump, &[label])
    }

    fn new_jump_unless_node(&self, cond: usize, t: &mut IlTree) -> usize {
        let label = self.new_get_node(t);
        t.new_node(IlKind::JumpUnless, &[label, cond])
    }
}

struct Labels {
    inner: Vec<Label>,
}

impl Labels {
    fn new() -> Labels {
        Labels { inner: vec![] }
    }

    fn new_label(&mut self, hint: &str, t: &mut IlTree) -> Label {
        let id = self.inner.len();
        let name = format!("{}_{}", hint, 1 + id);
        let ident_il = t.new_ident(name);
        let label = Label::new(ident_il);
        self.inner.push(label);
        label
    }

    fn new_labels_node(&self, t: &mut IlTree) -> usize {
        let children = self
            .inner
            .iter()
            .map(|label| label.ident_il)
            .collect::<Vec<_>>();
        t.new_node(IlKind::Labels, &children)
    }
}

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

fn gen_globals(symbols: &Symbols, t: &mut IlTree) -> usize {
    let mut globals = symbols
        .vars()
        .iter()
        .enumerate()
        .map(|(var_id, var)| (var_id, var.ident().to_owned()))
        .collect::<Vec<_>>();
    globals.sort();

    let mut children = vec![];
    for (_, ident) in globals {
        children.push(t.new_ident(ident));
    }

    t.new_node(IlKind::Globals, &children)
}

fn gen_expr(expr: &Expr, t: &mut IlTree, labels: &mut Labels, s: &SourceFileSystem) -> usize {
    let il = match expr.kind() {
        ExprKind::Lit(Lit::Bool(value)) => t.new_bool(*value),
        ExprKind::Lit(Lit::Int(value)) => t.new_int(*value),
        ExprKind::Prim(prim) => t.new_leaf(kind_from_prim(*prim)),
        ExprKind::Fun(_, ident) => {
            let ident = t.new_ident(ident.to_owned());
            t.new_node(IlKind::LabelGet, &[ident])
        }
        ExprKind::Global(_, ident) => {
            let ident = t.new_ident(ident.to_owned());
            t.new_node(IlKind::GlobalGet, &[ident])
        }
        ExprKind::Assign => match expr.children() {
            [left, right] => {
                let left = gen_expr(left, t, labels, s);
                let right = gen_expr(right, t, labels, s);
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
                children.push(gen_expr(child, t, labels, s));
            }
            t.new_node(kind, &children)
        }
        ExprKind::Do => {
            let mut children = vec![];
            for child in expr.children() {
                children.push(gen_expr(child, t, labels, s));
            }
            t.new_node(IlKind::Pop, &children)
        }
        ExprKind::If => match expr.children() {
            [cond, body, alt] => {
                let else_label = labels.new_label("else", t);
                let end_label = labels.new_label("end_if", t);

                let cond = gen_expr(cond, t, labels, s);
                let cond_with_jump = else_label.new_jump_unless_node(cond, t);

                let body = gen_expr(body, t, labels, s);
                let body_exit = end_label.new_jump_node(t);

                let alt_entrance = else_label.new_def_node(t);
                let alt = gen_expr(alt, t, labels, s);

                let end = end_label.new_def_node(t);

                t.new_node(
                    IlKind::Semi,
                    &[cond_with_jump, body, body_exit, alt_entrance, alt, end],
                )
            }
            _ => unimplemented!(),
        },
        ExprKind::Semi => {
            let mut children = vec![];
            for child in expr.children() {
                children.push(gen_expr(child, t, labels, s));
            }
            t.new_node(IlKind::Semi, &children)
        }
        ExprKind::FnDecl => match expr.children() {
            [ident, body] => {
                let (fun_label, end_label) = match ident.kind() {
                    ExprKind::Fun(_, ident) => (
                        labels.new_label(&ident, t),
                        labels.new_label(&format!("{}_exit", ident), t),
                    ),
                    _ => unreachable!(),
                };

                let skip = end_label.new_jump_node(t);
                let label_def = fun_label.new_def_node(t);
                let body = gen_expr(body, t, labels, s);
                let result = t.new_int(0);
                let ret = t.new_node(IlKind::Ret, &[result]);
                let end = end_label.new_def_node(t);

                t.new_node(IlKind::Semi, &[skip, label_def, body, ret, end])
            }
            _ => unreachable!(),
        },
        ExprKind::Ident(_) => unreachable!("名前解決で消えるはず"),
    };

    if expr.is_statement() || expr.is_decl() {
        set_text_comment(il, expr, t, s);
    }

    il
}

pub(crate) fn from_expr(expr: &Expr, symbols: &Symbols, s: &SourceFileSystem) -> IlTree {
    let mut il_tree = IlTree::new();
    let mut labels = Labels::new();

    let top_level = gen_expr(expr, &mut il_tree, &mut labels, s);
    let globals = gen_globals(symbols, &mut il_tree);
    let labels = labels.new_labels_node(&mut il_tree);
    let code_section = il_tree.new_node(IlKind::CodeSection, &[top_level]);
    let root = il_tree.new_node(IlKind::Root, &[globals, labels, code_section]);
    il_tree.set_root(root);

    il_tree
}
