use super::a_tree::*;
use crate::{context::Context, parse::parser::LambdaParserHost, syntax::syntax_token::SyntaxToken};

pub struct AstLambdaParserHost<'a> {
    pub(crate) context: &'a Context,
}

impl<'a> LambdaParserHost<'a> for AstLambdaParserHost<'a> {
    type AfterExpr = AExpr<'a>;
    type AfterDecl = ADecl<'a>;
    type AfterRoot = ARoot<'a>;

    fn after_number_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::Number(token)
    }

    fn after_ident_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::Var(token)
    }

    fn after_let_decl(
        &mut self,
        _keyword: SyntaxToken<'a>,
        name_opt: Option<SyntaxToken<'a>>,
        _equal_opt: Option<SyntaxToken<'a>>,
        _init_opt: Option<Self::AfterExpr>,
        _semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl {
        ADecl::Let(ALetDecl { name_opt })
    }

    fn after_root(
        &mut self,
        decls: Vec<Self::AfterDecl>,
        _eof: SyntaxToken<'a>,
    ) -> Self::AfterRoot {
        ARoot {
            decls: self.context.allocate_iter(decls),
        }
    }
}
