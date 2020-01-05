//! トークン

use crate::syntax::*;
use al_aux::syntax::{TokenKindTrait, TokenTrait};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TokenKind {
    /// 解釈できない文字
    Error,

    /// ファイルの末尾 (番兵)
    Eof,

    /// 空白やコメントなどの瑣末なトークン
    Tombstone,

    /// 識別子
    Ident,

    True,
    False,
    Assert,
    If,
    Else,
    Fn,

    /// 整数
    Int,

    /// `{`
    BraceL,

    /// `}`
    BraceR,

    /// `(`
    ParenL,

    /// `)`
    ParenR,

    /// `==`
    EqEq,

    /// `=`
    Eq,

    Minus,
    Plus,
    Slash,

    /// `*`
    Star,
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
    (TokenKind::If, "if"),
    (TokenKind::Else, "else"),
    (TokenKind::Fn, "fn"),
];

static SYMBOL_TEXTS: &[(TokenKind, &str)] = &[
    (TokenKind::BraceL, "{"),
    (TokenKind::BraceR, "}"),
    (TokenKind::ParenL, "("),
    (TokenKind::ParenR, ")"),
    (TokenKind::EqEq, "=="),
    (TokenKind::Eq, "="),
    (TokenKind::Minus, "-"),
    (TokenKind::Plus, "+"),
    (TokenKind::Slash, "/"),
    (TokenKind::Star, "*"),
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

impl TokenKindTrait for TokenKind {
    fn error() -> Self {
        TokenKind::Error
    }

    fn eof() -> Self {
        TokenKind::Eof
    }

    fn is_trivia(&self) -> bool {
        *self == TokenKind::Tombstone
    }
}

impl TokenTrait for Token {
    type Kind = TokenKind;

    fn kind(&self) -> TokenKind {
        self.kind()
    }
}
