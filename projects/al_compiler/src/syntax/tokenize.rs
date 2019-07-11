use crate::syntax::*;
use std::cell::Cell;

trait TokenKindTrait: Sized {
    fn error() -> Self;

    fn eof() -> Option<Self>;

    fn is_trivia(&self) -> bool;
}

trait TokenFactory: Sized {
    type Token;
    type Kind: TokenKindTrait;

    fn new_token(&self, kind: Self::Kind, start: usize, end: usize) -> Self::Token;
}

struct Tokenizer<'a, T: TokenFactory> {
    token_factory: T,
    text: &'a str,
    last: usize,
    current: usize,
    tokens: Vec<T::Token>,
    tick: Cell<usize>,
    stuck: bool,
}

impl<'a, T: TokenFactory> Tokenizer<'a, T> {
    fn new(token_factory: T, text: &'a str) -> Self {
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

    fn bump(&mut self) {
        if self.at_eof() {
            self.detect_infinite_loop();
            return;
        }

        self.current += self.text[self.current..].chars().next().unwrap().len_utf8();
    }

    fn add_token(&mut self, kind: T::Kind) {
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

    fn error(&mut self) {
        if self.stuck {
            self.bump();
            self.add_token(T::Kind::error());
        }
        self.stuck = true;
    }

    fn eof(&mut self) {
        if let Some(kind) = T::Kind::eof() {
            self.add_token(kind);
        }
    }

    fn tokenize(mut self, any: impl Fn(&mut Self)) -> Vec<T::Token> {
        while !self.at_eof() {
            any(&mut self);
            self.error();
        }
        self.eof();
        self.tokens
    }
}

impl TokenKindTrait for TokenKind {
    fn error() -> Self {
        TokenKind::Error
    }

    fn eof() -> Option<Self> {
        Some(TokenKind::Eof)
    }

    fn is_trivia(&self) -> bool {
        *self == TokenKind::Tombstone
    }
}

struct AlteryTokenFactory<'a> {
    file: usize,
    text: &'a str,
}

impl<'a> AlteryTokenFactory<'a> {
    fn new(file: usize, text :&'a str) -> Self {
        AlteryTokenFactory {file,text}
    }
}

impl<'a> TokenFactory for AlteryTokenFactory<'a> {
    type Token = Token;
    type Kind = TokenKind;

    fn new_token(&self, kind: TokenKind, start: usize, end: usize) -> Token {
        let text = self.text[start..end].to_string();
        let location = SourceLocation::new(self.file, start, end);
        Token::new(kind, text, location)
    }
}

type Tok<'a> = Tokenizer<'a, AlteryTokenFactory<'a>>;

fn spaces(t: &mut Tok<'_>) {
    if t.eat_while(|c| c.is_ascii_whitespace()) {
        t.add_token(TokenKind::Tombstone);
    }
}

fn ident(t: &mut Tok<'_>) {
    if !t.next().is_ascii_digit() && t.eat_while(|c| c.is_ascii_alphanumeric() || c == b'_') {
        let kind = ident_kind(t.current_text());
        t.add_token(kind);
    }
}

fn int(t: &mut Tok<'_>) {
    if t.eat_while(|c| c.is_ascii_digit()) {
        t.add_token(TokenKind::Int);
    }
}

fn symbol(t: &mut Tok<'_>) {
    for &(kind, text) in TokenKind::symbol_texts() {
        if t.eat(text) {
            t.add_token(kind);
        }
    }
}

fn any(t: &mut Tok<'_>) {
    spaces(t);
    ident(t);
    int(t);
    symbol(t);
}

fn ident_kind(text: &str) -> TokenKind {
    for &(kind, keyword_text) in TokenKind::keyword_texts() {
        if text == keyword_text {
            return kind;
        }
    }
    TokenKind::Ident
}

pub(crate) fn tokenize(file: usize, text: &str) -> Vec<Token> {
    let token_factory = AlteryTokenFactory::new(file, text);
    Tokenizer::new(token_factory, text).tokenize(any)
}
