use crate::syntax::*;
use std::cell::Cell;

struct Tokenizer<'a> {
    file: usize,
    text: &'a str,
    last: usize,
    current: usize,
    tokens: Vec<Token>,
    tick: Cell<usize>,
    stuck: bool,
}

impl<'a> Tokenizer<'a> {
    fn new(file: usize, text: &'a str) -> Self {
        Tokenizer {
            file,
            text,
            last: 0,
            current: 0,
            tokens: Vec::with_capacity(512),
            tick: Cell::default(),
            stuck: false,
        }
    }

    fn bump(&mut self) {
        if self.at_eof() {
            self.detect_infinite_loop();
            return;
        }

        self.current += self.text[self.current..].chars().next().unwrap().len_utf8();
    }

    fn add_token(&mut self, kind: TokenKind) {
        assert!(self.last <= self.current && self.current <= self.text.len());

        let text = self.current_text().to_string();
        let location = SourceLocation::new(self.file, self.last, self.current);

        if kind != TokenKind::Tombstone {
            self.tokens.push(Token::new(kind, text, location));
        }

        self.last = self.current;
        self.stuck = false;
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get() + 1;

        assert!(tick < 10_000_000);
        self.tick.set(tick);
    }

    fn at_eof(&self) -> bool {
        self.current >= self.text.len()
    }

    fn next(&self) -> u8 {
        self.detect_infinite_loop();

        if self.at_eof() {
            return 0;
        }

        self.text.as_bytes()[self.current]
    }

    fn ate(&self) -> bool {
        self.last < self.current
    }

    fn current_text(&self) -> &str {
        &self.text[self.last..self.current]
    }

    fn eat_while(&mut self, pred: impl Fn(u8) -> bool) -> bool {
        while !self.at_eof() && pred(self.next()) {
            self.bump();
        }
        self.ate()
    }

    fn eat(&mut self, prefix: &str) -> bool {
        if self.text[self.current..].starts_with(prefix) {
            self.current += prefix.len();
        }
        self.ate()
    }

    fn spaces(&mut self) {
        if self.eat_while(|c| c.is_ascii_whitespace()) {
            self.add_token(TokenKind::Tombstone);
        }
    }

    fn ident(&mut self) {
        if !self.next().is_ascii_digit()
            && self.eat_while(|c| c.is_ascii_alphanumeric() || c == b'_')
        {
            let kind = ident_kind(self.current_text());
            self.add_token(kind);
        }
    }

    fn symbol(&mut self) {
        for &(kind, text) in TokenKind::symbol_texts() {
            if self.eat(text) {
                self.add_token(kind);
            }
        }
    }

    fn error(&mut self) {
        if self.stuck {
            self.bump();
            self.add_token(TokenKind::Error);
        }
        self.stuck = true;
    }

    fn eof(&mut self) {
        self.add_token(TokenKind::Eof);
    }

    fn tokenize(mut self) -> Vec<Token> {
        while !self.at_eof() {
            self.spaces();
            self.ident();
            self.symbol();
            self.error();
        }
        self.eof();
        self.tokens
    }
}

fn ident_kind(text: &str) -> TokenKind {
    for &(kind, keyword_text) in TokenKind::keyword_texts() {
        if text == keyword_text {
            return kind;
        }
    }
    TokenKind::Ident
}

pub(crate) fn tokenize(file: usize, source: &str) -> Vec<Token> {
    Tokenizer::new(file, source).tokenize()
}
