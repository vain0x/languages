use crate::syntax::*;
use al_aux::il::*;

fn gen_expr(ast: &Ast, t: &mut IlTree) -> usize {
    match ast.kind() {
        AstKind::True => t.add_bool(true),
        AstKind::False => t.add_bool(false),
        AstKind::Assert | AstKind::Ident(_) => unimplemented!(),
        AstKind::Int(value) => t.add_int(*value),
        AstKind::Add | AstKind::Sub | AstKind::Mul | AstKind::Div | AstKind::Eq => {
            let kind = match ast.kind() {
                AstKind::Add => IlKind::OpAdd,
                AstKind::Sub => IlKind::OpSub,
                AstKind::Mul => IlKind::OpMul,
                AstKind::Div => IlKind::OpDiv,
                AstKind::Eq => IlKind::OpEq,
                _ => unreachable!(),
            };
            let mut children = vec![];
            for child in ast.children() {
                children.push(gen_expr(child, t));
            }
            t.add_node(kind, &children)
        }
        AstKind::Call => match ast.children() {
            [cal, arg] => match cal.kind() {
                AstKind::Assert => {
                    let arg = gen_expr(&arg, t);
                    t.add_node(IlKind::Assert, &[arg])
                }
                _ => unimplemented!("{:?}", cal.kind()),
            },
            _ => unreachable!(),
        },
        AstKind::Semi => {
            let mut children = vec![];
            for child in ast.children() {
                children.push(gen_expr(child, t));
            }
            t.add_node(IlKind::Semi, &children)
        }
    }
}

pub(crate) fn gen(ast: &Ast) -> IlTree {
    let mut il_tree = IlTree::new();

    let main = gen_expr(ast, &mut il_tree);
    let exit = il_tree.add_leaf(IlKind::Exit);
    let root = il_tree.add_node(IlKind::Semi, &[main, exit]);
    il_tree.set_root(root);

    il_tree
}
