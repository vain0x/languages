use crate::{token_kind_trait::TokenKindTrait, token_with_trivia::TokenWithTrivia};

pub trait TokenStreamHost {
    type TokenKind: TokenKindTrait;

    fn advance(&mut self) -> (Self::TokenKind, usize);
}

pub struct TokenStream<H: TokenStreamHost> {
    lookahead: (H::TokenKind, usize),
    pub host: H,
}

impl<H: TokenStreamHost> TokenStream<H> {
    pub fn new(mut host: H) -> Self {
        let lookahead = host.advance();

        TokenStream { lookahead, host }
    }

    pub fn advance(&mut self) -> TokenWithTrivia<H::TokenKind> {
        let mut leading_len = 0_usize;
        while self.lookahead.0.is_leading_trivia() {
            leading_len += self.lookahead.1;
            self.lookahead = self.host.advance();
        }

        let (kind, body_len) = self.lookahead;
        self.lookahead = self.host.advance();

        let mut trailing_len = 0_usize;
        while self.lookahead.0.is_trailing_trivia() {
            trailing_len += self.lookahead.1;
            self.lookahead = self.host.advance();
        }

        TokenWithTrivia {
            kind,
            leading_len,
            body_len,
            trailing_len,
        }
    }
}
