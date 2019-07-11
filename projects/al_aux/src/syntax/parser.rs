use crate::syntax::*;
use std::cell::Cell;

pub trait TokenTrait {
    type Kind: TokenKindTrait + PartialEq;

    fn kind(&self) -> Self::Kind;
}

pub struct TokenParser<'a, Token> {
    tokens: &'a [Token],
    current: usize,
    tick: Cell<usize>,
}

impl<'a, Token: TokenTrait> TokenParser<'a, Token> {
    pub fn new(tokens: &'a [Token]) -> Self {
        TokenParser {
            tokens,
            current: 0,
            tick: Cell::default(),
        }
    }

    pub fn bump(&mut self) {
        if self.at_eof() {
            return;
        }

        assert!(self.current + 1 < self.tokens.len());
        self.current += 1;
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get() + 1;

        assert!(tick < 10_000_000);
        self.tick.set(tick);
    }

    pub fn next_token(&self) -> &Token {
        self.detect_infinite_loop();

        &self.tokens[self.current]
    }

    pub fn next(&self) -> Token::Kind {
        self.next_token().kind()
    }

    pub fn at_eof(&self) -> bool {
        self.at(Token::Kind::eof())
    }

    pub fn at(&self, kind: Token::Kind) -> bool {
        self.next() == kind
    }
}
