use crate::{ast::a_tree::Ast, context::Context};

pub(crate) struct SyntaxTree<'a> {
    pub(crate) ast: Ast<'a>,
    pub(crate) context: &'a Context,
}
