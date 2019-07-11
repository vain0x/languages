use std::cell::Cell;

pub trait TokenKindTrait: Sized {
    fn error() -> Self;

    fn eof() -> Self;

    fn is_trivia(&self) -> bool;
}

pub trait TokenFactoryTrait: Sized {
    type Token;
    type Kind: TokenKindTrait;

    fn new_token(&self, kind: Self::Kind, start: usize, end: usize) -> Self::Token;
}

pub struct Tokenizer<'a, T: TokenFactoryTrait> {
    token_factory: T,
    text: &'a str,
    last: usize,
    current: usize,
    tokens: Vec<T::Token>,
    tick: Cell<usize>,
    stuck: bool,
}

impl<'a, T: TokenFactoryTrait> Tokenizer<'a, T> {
    pub fn new(token_factory: T, text: &'a str) -> Self {
        Tokenizer {
            token_factory,
            text,
            last: 0,
            current: 0,
            tokens: Vec::with_capacity(512),
            tick: Cell::default(),
            stuck: false,
        }
    }

    pub fn bump(&mut self) {
        if self.at_eof() {
            self.detect_infinite_loop();
            return;
        }

        self.current += self.text[self.current..].chars().next().unwrap().len_utf8();
    }

    pub fn add_token(&mut self, kind: T::Kind) {
        assert!(self.last <= self.current && self.current <= self.text.len());

        if !T::Kind::is_trivia(&kind) {
            let token = self.token_factory.new_token(kind, self.last, self.current);
            self.tokens.push(token);
        }

        self.last = self.current;
        self.stuck = false;
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get() + 1;

        assert!(tick < 10_000_000);
        self.tick.set(tick);
    }

    pub fn at_eof(&self) -> bool {
        self.current >= self.text.len()
    }

    pub fn next(&self) -> u8 {
        self.detect_infinite_loop();

        if self.at_eof() {
            return 0;
        }

        self.text.as_bytes()[self.current]
    }

    fn ate(&self) -> bool {
        self.last < self.current
    }

    pub fn current_text(&self) -> &str {
        &self.text[self.last..self.current]
    }

    pub fn eat_while(&mut self, pred: impl Fn(u8) -> bool) -> bool {
        while !self.at_eof() && pred(self.next()) {
            self.bump();
        }
        self.ate()
    }

    pub fn eat(&mut self, prefix: &str) -> bool {
        if self.text[self.current..].starts_with(prefix) {
            self.current += prefix.len();
        }
        self.ate()
    }

    fn error(&mut self) {
        if self.stuck {
            self.bump();
            self.add_token(T::Kind::error());
        }
        self.stuck = true;
    }

    fn eof(&mut self) {
        self.add_token(T::Kind::eof());
    }

    pub fn tokenize(mut self, any: impl Fn(&mut Self)) -> Vec<T::Token> {
        while !self.at_eof() {
            any(&mut self);
            self.error();
        }
        self.eof();
        self.tokens
    }
}
