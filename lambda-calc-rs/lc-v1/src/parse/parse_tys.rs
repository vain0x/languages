use super::{parser::LambdaParser, parser_host::LambdaParserHost};
use crate::{syntax::syntax_token::SyntaxToken, token::token_kind::TokenKind};

impl<'a, 'h, H: LambdaParserHost<'a>> LambdaParser<'a, 'h, H> {
    fn parse_fn_ty(&mut self) -> H::AfterTy {
        let keyword = self.bump();

        let left_paren_opt = self.eat(TokenKind::LeftParen);
        let param_ty_list_opt = if let Some(left_paren) = left_paren_opt {
            let mut param_ty_list = self.host.before_param_ty_list(left_paren);

            loop {
                match self.next() {
                    TokenKind::Eof
                    | TokenKind::RightParen
                    | TokenKind::RightBrace
                    | TokenKind::SemiColon => break,
                    _ => {}
                }

                let param_ty = match self.parse_ty() {
                    Some(it) => it,
                    None => {
                        eprintln!("expected ty");
                        self.skip();
                        continue;
                    }
                };
                let comma_opt = self.eat(TokenKind::Comma);

                self.host
                    .after_param_ty(param_ty, comma_opt, &mut param_ty_list);
            }

            let right_paren_opt = self.eat(TokenKind::RightParen);
            Some(
                self.host
                    .after_param_ty_list(right_paren_opt, param_ty_list),
            )
        } else {
            None
        };
        let (arrow_opt, result_ty_opt) = self.parse_result_ty();

        self.host
            .after_fn_ty(keyword, param_ty_list_opt, arrow_opt, result_ty_opt)
    }

    pub(crate) fn parse_ty(&mut self) -> Option<H::AfterTy> {
        let ty = match self.next() {
            TokenKind::Fn => self.parse_fn_ty(),
            TokenKind::Ident => {
                let name = self.bump();
                self.host.after_name_ty(name)
            }
            _ => return None,
        };
        Some(ty)
    }

    pub(crate) fn parse_ty_ascription(&mut self) -> (Option<SyntaxToken<'a>>, Option<H::AfterTy>) {
        let colon_opt = self.eat(TokenKind::Colon);
        let ty_opt = if colon_opt.is_some() {
            self.parse_ty()
        } else {
            None
        };
        (colon_opt, ty_opt)
    }

    pub(crate) fn parse_result_ty(&mut self) -> (Option<SyntaxToken<'a>>, Option<H::AfterTy>) {
        let arrow_opt = self.eat(TokenKind::MinusRight);
        let result_ty_opt = if arrow_opt.is_some() {
            self.parse_ty()
        } else {
            None
        };
        (arrow_opt, result_ty_opt)
    }
}
