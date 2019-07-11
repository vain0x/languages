use crate::syntax::*;
use al_aux::syntax::*;

type Tok<'a> = Tokenizer<'a, TokenFactory<'a>>;

struct TokenFactory<'a> {
    file: usize,
    text: &'a str,
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

impl<'a> TokenFactory<'a> {
    fn new(file: usize, text: &'a str) -> Self {
        TokenFactory { file, text }
    }
}

impl<'a> TokenFactoryTrait for TokenFactory<'a> {
    type Token = Token;
    type Kind = TokenKind;

    fn new_token(&self, kind: TokenKind, start: usize, end: usize) -> Token {
        let text = self.text[start..end].to_string();
        let location = SourceLocation::new(self.file, start, end);
        Token::new(kind, text, location)
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

pub(crate) fn tokenize(file: usize, text: &str) -> Vec<Token> {
    let token_factory = TokenFactory::new(file, text);
    Tokenizer::new(token_factory, text).tokenize(any)
}
