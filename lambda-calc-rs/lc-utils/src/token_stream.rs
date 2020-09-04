use crate::{token_kind_trait::TokenKindTrait, token_with_trivia::TokenWithTrivia};

pub trait TokenStreamHost {
    type TokenKind: TokenKindTrait;

    fn next(&mut self) -> (Self::TokenKind, usize);
}

pub struct TokenStream<'h, H: TokenStreamHost> {
    lookahead: (H::TokenKind, usize),
    host: &'h mut H,
}

impl<'h, H: TokenStreamHost> TokenStream<'h, H> {
    pub fn new(host: &'h mut H) -> Self {
        let lookahead = host.next();

        TokenStream { lookahead, host }
    }

    pub fn next(&mut self) -> Option<TokenWithTrivia<H::TokenKind>> {
        if self.lookahead.0.is_eof() {
            return None;
        }

        let mut leading_len = 0_usize;
        while self.lookahead.0.is_leading_trivia() {
            leading_len += self.lookahead.1;
            self.lookahead = self.host.next();
        }

        let (kind, body_len) = self.lookahead;
        self.lookahead = self.host.next();

        let mut trailing_len = 0_usize;
        while self.lookahead.0.is_trailing_trivia() {
            trailing_len += self.lookahead.1;
            self.lookahead = self.host.next();
        }

        Some(TokenWithTrivia {
            kind,
            leading_len,
            body_len,
            trailing_len,
        })
    }
}
