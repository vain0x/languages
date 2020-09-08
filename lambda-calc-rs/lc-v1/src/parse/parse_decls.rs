use super::parser::{LambdaParser, LambdaParserHost};
use crate::token::token_kind::TokenKind;

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    fn parse_let_decl(&mut self) -> H::AfterDecl {
        let keyword = self.bump();

        let name_opt = self.eat(TokenKind::Ident);
        // let (colon_opt, ty_opt) = self.parse_ty_ascription();

        let equal_opt = self.eat(TokenKind::Equal);
        let init_opt = if equal_opt.is_some() {
            self.parse_expr()
        } else {
            None
        };

        let semi_opt = self.eat(TokenKind::SemiColon);
        self.host
            .after_let_decl(keyword, name_opt, equal_opt, init_opt, semi_opt)
    }

    pub(crate) fn parse_decl(&mut self) -> Option<H::AfterDecl> {
        let decl = match self.next() {
            TokenKind::Let => self.parse_let_decl(),
            _ => match self.parse_expr() {
                Some(expr) => {
                    let semi_opt = self.eat(TokenKind::SemiColon);
                    self.host.after_expr_decl(expr, semi_opt)
                }
                None => return None,
            },
        };
        Some(decl)
    }
}
