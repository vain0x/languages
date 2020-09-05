use super::a_tree::*;
use crate::{
    context::Context, parse::parser::LambdaParserHost, syntax::syntax_token::SyntaxToken, utils::*,
};

pub struct AstLambdaParserHost<'a> {
    pub(crate) context: &'a Context,
}

impl<'a> LambdaParserHost<'a> for AstLambdaParserHost<'a> {
    type BeforeArgList = BumpaloVec<'a, AExpr<'a>>;
    type AfterArgList = BumpaloVec<'a, AExpr<'a>>;

    type AfterExpr = AExpr<'a>;
    type AfterDecl = ADecl<'a>;
    type AfterRoot = ARoot<'a>;

    fn before_arg_list(&mut self, _open_paren: SyntaxToken<'a>) -> Self::BeforeArgList {
        BumpaloVec::new_in(&self.context.bump)
    }

    fn after_arg(
        &mut self,
        expr: Self::AfterExpr,
        _comma_opt: Option<SyntaxToken<'a>>,
        arg_list: &mut Self::BeforeArgList,
    ) {
        arg_list.push(expr);
    }

    fn after_arg_list(
        &mut self,
        _close_paren: Option<SyntaxToken<'a>>,
        arg_list: Self::BeforeArgList,
    ) -> Self::AfterArgList {
        arg_list
    }

    fn after_number_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::Number(token)
    }

    fn after_ident_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::Var(token)
    }

    fn after_call_expr(
        &mut self,
        callee: Self::AfterExpr,
        arg_list: Self::AfterArgList,
    ) -> Self::AfterExpr {
        AExpr::Call(ACallExpr {
            callee: BumpaloBox::new_in(callee, &self.context.bump),
            args: arg_list,
        })
    }

    fn after_let_decl(
        &mut self,
        _keyword: SyntaxToken<'a>,
        name_opt: Option<SyntaxToken<'a>>,
        _equal_opt: Option<SyntaxToken<'a>>,
        init_opt: Option<Self::AfterExpr>,
        _semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl {
        ADecl::Let(ALetDecl { name_opt, init_opt })
    }

    fn after_root(&mut self, decls: Vec<Self::AfterDecl>, eof: SyntaxToken<'a>) -> Self::AfterRoot {
        ARoot {
            decls: self.context.allocate_iter(decls),
            eof,
        }
    }
}
