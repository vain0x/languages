#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Keyword {
    Let,
    Fun,
    If,
    Else,
    While,
}

impl Keyword {
    pub(crate) fn text(self) -> &'static str {
        match self {
            Keyword::Let => "let",
            Keyword::Fun => "fun",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
        }
    }

    pub(crate) fn get_all() -> &'static [Keyword] {
        &[
            Keyword::Let,
            Keyword::Fun,
            Keyword::If,
            Keyword::Else,
            Keyword::While,
        ]
    }
}
