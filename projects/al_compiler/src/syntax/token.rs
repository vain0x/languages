use crate::syntax::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TokenKind {
    Error,
    Eof,
    Tombstone,
    Ident,
    True,
    False,
    Assert,
    Int,
    ParenL,
    ParenR,
    EqEq,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Token {
    kind: TokenKind,
    text: String,
    loc: SourceLocation,
}

static KEYWORD_TEXTS: &[(TokenKind, &str)] = &[
    (TokenKind::True, "true"),
    (TokenKind::False, "false"),
    (TokenKind::Assert, "assert"),
];

static SYMBOL_TEXTS: &[(TokenKind, &str)] = &[
    (TokenKind::ParenL, "("),
    (TokenKind::ParenR, ")"),
    (TokenKind::EqEq, "=="),
];

impl TokenKind {
    pub(crate) fn keyword_texts() -> &'static [(TokenKind, &'static str)] {
        KEYWORD_TEXTS
    }

    pub(crate) fn symbol_texts() -> &'static [(TokenKind, &'static str)] {
        SYMBOL_TEXTS
    }
}

impl Token {
    pub(crate) fn new(kind: TokenKind, text: String, loc: SourceLocation) -> Self {
        Token { kind, text, loc }
    }

    pub(crate) fn kind(&self) -> TokenKind {
        self.kind
    }

    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    pub(crate) fn loc(&self) -> SourceLocation {
        self.loc
    }
}
