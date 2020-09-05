use lc_utils::token_kind_trait::TokenKindTrait;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum TokenKind {
    Eof,
    NewLine,
    Blank,
    Comment,
    BadToken,
    Number,
    Ident,
    Fn,
    Let,
    OpenParen,
    CloseParen,
    Comma,
    Equal,
    SemiColon,
}

impl TokenKind {
    pub(crate) fn from_ident(s: &str) -> TokenKind {
        match s {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            _ => TokenKind::Ident,
        }
    }
}

impl TokenKindTrait for TokenKind {
    fn is_eof(self) -> bool {
        self == TokenKind::Eof
    }

    fn is_leading_trivia(self) -> bool {
        match self {
            TokenKind::NewLine | TokenKind::Blank | TokenKind::Comment | TokenKind::BadToken => {
                true
            }
            _ => false,
        }
    }

    fn is_trailing_trivia(self) -> bool {
        match self {
            TokenKind::Blank | TokenKind::Comment | TokenKind::BadToken => true,
            _ => false,
        }
    }
}
