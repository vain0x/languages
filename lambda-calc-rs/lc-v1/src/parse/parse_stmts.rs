use super::{parser::LambdaParser, parser_host::LambdaParserHost};
use crate::token::token_kind::TokenKind;

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    fn parse_let_stmt(&mut self) -> H::AfterStmt {
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
            .after_let_stmt(keyword, name_opt, equal_opt, init_opt, semi_opt)
    }

    pub(crate) fn parse_stmt(&mut self) -> Option<H::AfterStmt> {
        let stmt = match self.next() {
            TokenKind::Let => self.parse_let_stmt(),
            _ => match self.parse_expr() {
                Some(expr) => {
                    let semi_opt = self.eat(TokenKind::SemiColon);
                    self.host.after_expr_stmt(expr, semi_opt)
                }
                None => return None,
            },
        };
        Some(stmt)
    }
}
