use super::parser::{LambdaParser, LambdaParserHost};
use crate::token::token_kind::TokenKind;
use lc_utils::parser::Parser;

fn parse_atomic_expr<'a, H: LambdaParserHost<'a>>(
    px: &mut LambdaParser<'a, H>,
) -> Option<H::AfterExpr> {
    let expr = match px.next() {
        TokenKind::Number => {
            let token = px.bump();
            px.host.after_number_expr(token)
        }
        TokenKind::Ident => {
            let token = px.bump();
            px.host.after_ident_expr(token)
        }
        // TokenKind::LeftParen => {
        //     let left_paren = px.bump();
        //     match px.next() {
        //         TokenKind::RightParen => {
        //             let right_paren = px.bump();
        //             alloc_unit_expr_from_parens(event, left_paren, right_paren, px)
        //         }
        //         _ => {
        //             let body_opt = parse_expr(px);
        //             let right_paren_opt = px.eat(TokenKind::RightParen);
        //             alloc_paren_expr(event, left_paren, body_opt, right_paren_opt, px)
        //         }
        //     }
        // }
        _ => return None,
    };
    Some(expr)
}

fn parse_suffix_expr<'a, H: LambdaParserHost<'a>>(
    px: &mut LambdaParser<'a, H>,
) -> Option<H::AfterExpr> {
    let mut left = parse_atomic_expr(px)?;

    loop {
        match px.next() {
            // TokenKind::OpenParen => {
            //     let event = px.start_parent(&left.1);
            //     let left_paren = px.bump();
            //     let arg_list = parse_tuple_arg_list(left_paren, px);
            //     left = alloc_call_expr(event, left, arg_list, px);
            // }
            _ => return Some(left),
        }
    }
}

pub(crate) fn parse_expr<'a, H: LambdaParserHost<'a>>(
    px: &mut LambdaParser<'a, H>,
) -> Option<H::AfterExpr> {
    parse_suffix_expr(px)
}
