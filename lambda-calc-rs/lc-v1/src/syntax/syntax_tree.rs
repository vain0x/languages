use crate::{ast::a_tree::Ast, context::Context};

#[allow(unused)]
pub(crate) struct SyntaxTree<'a> {
    pub(crate) ast: Ast<'a>,
    pub(crate) context: &'a Context,
}
