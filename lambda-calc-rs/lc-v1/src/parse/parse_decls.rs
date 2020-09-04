// use super::{parse_exprs::parse_expr, parser::LambdaParser};
// use crate::token::token_kind::TokenKind;

// fn parse_let_decl<Px: LambdaParser>(px: &mut Px) -> Px::AfterDecl {
//     let event = px.start_element();
//     let keyword = px.bump();

//     let name_opt = px.eat(TokenKind::Ident);
//     // let (colon_opt, ty_opt) = parse_ty_ascription(px);

//     let equal_opt = px.eat(TokenKind::Equal);
//     let init_opt = if equal_opt.is_some() {
//         parse_expr(px)
//     } else {
//         None
//     };

//     let semi_opt = px.eat(TokenKind::SemiColon);
//     px.after_let_decl(event, keyword, name_opt, equal_opt, init_opt, semi_opt)
// }

// pub(crate) fn parse_decl<Px: LambdaParser>(px: &mut Px) -> Option<Px::AfterDecl> {
//     let decl = match px.next() {
//         TokenKind::Let => parse_let_decl(px),
//         _ => return None,
//     };
//     Some(decl)
// }
