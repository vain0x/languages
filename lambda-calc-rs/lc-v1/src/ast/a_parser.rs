use super::a_tree::*;
use crate::{context::Context, parse::parser::LambdaParserHost, syntax::syntax_token::SyntaxToken};

pub struct AstLambdaParserHost<'a> {
    pub(crate) context: &'a Context,
}

impl<'a> LambdaParserHost for AstLambdaParserHost<'a> {
    type TokenData = SyntaxToken<'a>;

    type AfterExpr = AExpr<'a>;
    type AfterDecl = ADecl<'a>;
    type AfterRoot = ARoot<'a>;

    fn after_number_expr(&mut self, token: Self::TokenData) -> Self::AfterExpr {
        AExpr::Number(token)
    }

    fn after_ident_expr(&mut self, token: Self::TokenData) -> Self::AfterExpr {
        AExpr::Var(token)
    }

    fn after_let_decl(
        &mut self,
        keyword: Self::TokenData,
        name_opt: Option<Self::TokenData>,
        equal_opt: Option<Self::TokenData>,
        init_opt: Option<Self::AfterExpr>,
        semi_opt: Option<Self::TokenData>,
    ) -> Self::AfterDecl {
        todo!()
    }

    fn after_root(&mut self, eof: Self::TokenData) -> Self::AfterRoot {
        todo!()
    }
}

// impl AParser

// struct StartEvent;
// type EndEvent = ();

// type AfterExpr<'a> = AExpr<'a>;
// type AfterDecl<'a> = ADecl<'a>;
// type AfterRoot<'a> = ARoot<'a>;

// fn after_number_expr(_event: StartEvent, token: SyntaxToken, host: &A) -> AfterExpr {
//     AExpr::Number(token.)
// }

// fn after_ident_expr(&mut self, _event: StartEvent, token: SyntaxToken) -> AfterExpr {}

// fn after_let_decl(
//     &mut self,
//     _event: Self::StartEvent,
//     keyword: SyntaxToken,
//     name_opt: Option<SyntaxToken>,
//     equal_opt: Option<SyntaxToken>,
//     init_opt: Option<Self::AfterExpr>,
//     semi_opt: Option<SyntaxToken>,
// ) -> Self::AfterDecl;

// fn after_root(&mut self, _event: Self::StartEvent, eof: SyntaxToken) -> Self::AfterRoot;
