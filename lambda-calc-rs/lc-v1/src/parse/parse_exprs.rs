use super::parser::{LambdaParser, LambdaParserHost};
use crate::{syntax::syntax_token::SyntaxToken, token::token_kind::TokenKind};
use lc_utils::parser::Parser;

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    fn parse_tuple_param_list(&mut self, open_paren: SyntaxToken<'a>) -> H::AfterParamList {
        let mut param_list = self.host.before_param_list(open_paren);

        loop {
            let name = match self.next() {
                TokenKind::Eof | TokenKind::CloseParen | TokenKind::SemiColon => break,
                TokenKind::Ident => self.bump(),
                _ => {
                    eprintln!("expected expr");
                    self.skip();
                    continue;
                }
            };

            let comma_opt = self.eat(TokenKind::Comma);
            self.host.after_param(name, comma_opt, &mut param_list);
        }

        let close_paren_opt = self.eat(TokenKind::CloseParen);

        self.host.after_param_list(close_paren_opt, param_list)
    }

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

    fn parse_paren_expr(&mut self) -> Option<H::AfterExpr> {
        let _open_paren = self.bump();
        let body_opt = self.parse_expr();
        let _close_paren_op = self.eat(TokenKind::CloseParen);
        // FIXME: self.host.after_paren_expr(open_paren, body_opt, close_paren_opt);
        body_opt
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
            TokenKind::OpenParen => return self.parse_paren_expr(),
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

    fn parse_fn_expr(&mut self) -> H::AfterExpr {
        let keyword = self.bump();

        let param_list_opt = match self.eat(TokenKind::OpenParen) {
            Some(open_paren) => Some(self.parse_tuple_param_list(open_paren)),
            None => None,
        };

        let body_opt = self.parse_expr();
        self.host.after_fn_expr(keyword, param_list_opt, body_opt)
    }

    pub(crate) fn parse_expr(&mut self) -> Option<H::AfterExpr> {
        match self.next() {
            TokenKind::Eof | TokenKind::CloseParen | TokenKind::Comma | TokenKind::SemiColon => {
                None
            }
            TokenKind::Fn => Some(self.parse_fn_expr()),
            _ => self.parse_suffix_expr(),
        }
    }
}
