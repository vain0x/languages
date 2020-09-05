use super::parser::{LambdaParser, LambdaParserHost};
use crate::{syntax::syntax_token::SyntaxToken, token::token_kind::TokenKind};
use lc_utils::parser::Parser;

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    fn parse_tuple_arg_list(&mut self, open_paren: SyntaxToken<'a>) -> H::AfterArgList {
        let mut arg_list = self.host.before_arg_list(open_paren);

        loop {
            match self.next() {
                TokenKind::Eof | TokenKind::CloseParen | TokenKind::SemiColon => break,
                TokenKind::Comma => {
                    self.skip();
                    continue;
                }
                _ => {}
            }

            let expr = match self.parse_expr() {
                Some(it) => it,
                None => {
                    eprintln!("expected expr");
                    self.skip();
                    continue;
                }
            };

            let comma_opt = self.eat(TokenKind::Comma);
            self.host.after_arg(expr, comma_opt, &mut arg_list);
        }

        let close_paren_opt = self.eat(TokenKind::CloseParen);

        self.host.after_arg_list(close_paren_opt, arg_list)
    }

    fn parse_atomic_expr(&mut self) -> Option<H::AfterExpr> {
        let expr = match self.next() {
            TokenKind::Number => {
                let token = self.bump();
                self.host.after_number_expr(token)
            }
            TokenKind::Ident => {
                let token = self.bump();
                self.host.after_ident_expr(token)
            }
            // TokenKind::LeftParen => {
            //     let left_paren = self.bump();
            //     match self.next() {
            //         TokenKind::RightParen => {
            //             let right_paren = self.bump();
            //             alloc_unit_expr_from_parens(event, left_paren, right_paren, px)
            //         }
            //         _ => {
            //             let body_opt = parse_expr(px);
            //             let right_paren_opt = self.eat(TokenKind::RightParen);
            //             alloc_paren_expr(event, left_paren, body_opt, right_paren_opt, px)
            //         }
            //     }
            // }
            _ => return None,
        };
        Some(expr)
    }

    fn parse_suffix_expr(&mut self) -> Option<H::AfterExpr> {
        let mut left = self.parse_atomic_expr()?;

        loop {
            match self.next() {
                TokenKind::OpenParen => {
                    let left_paren = self.bump();
                    let arg_list = self.parse_tuple_arg_list(left_paren);
                    left = self.host.after_call_expr(left, arg_list);
                }
                _ => return Some(left),
            }
        }
    }

    pub(crate) fn parse_expr(&mut self) -> Option<H::AfterExpr> {
        self.parse_suffix_expr()
    }
}
