use super::{
    parse_exprs::parse_expr,
    parser::{LambdaParser, LambdaParserHost},
};
use crate::token::token_kind::TokenKind;
use lc_utils::parser::Parser;

fn parse_let_decl<H: LambdaParserHost>(px: &mut LambdaParser<H>) -> H::AfterDecl {
    let keyword = px.bump();

    let name_opt = px.eat(TokenKind::Ident);
    // let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::SemiColon);
    px.host
        .after_let_decl(keyword, name_opt, equal_opt, init_opt, semi_opt)
}

pub(crate) fn parse_decl<H: LambdaParserHost>(px: &mut LambdaParser<H>) -> Option<H::AfterDecl> {
    let decl = match px.next() {
        TokenKind::Let => parse_let_decl(px),
        _ => return None,
    };
    Some(decl)
}
