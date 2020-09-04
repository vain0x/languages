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
    Equal,
    SemiColon,
}

impl TokenKind {
    pub(crate) fn is_leading_trivia(self) -> bool {
        match self {
            TokenKind::NewLine | TokenKind::Blank | TokenKind::Comment | TokenKind::BadToken => {
                true
            }
            _ => false,
        }
    }

    pub(crate) fn is_trailing_trivia(self) -> bool {
        match self {
            TokenKind::Blank | TokenKind::Comment | TokenKind::BadToken => true,
            _ => false,
        }
    }

    pub(crate) fn from_ident(s: &str) -> TokenKind {
        match s {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            _ => TokenKind::Ident,
        }
    }
}
