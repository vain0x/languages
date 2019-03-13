#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Keyword {
    Let,
    Fun,
    Return,
    If,
    Else,
    While,
}

impl Keyword {
    pub(crate) fn text(self) -> &'static str {
        match self {
            Keyword::Let => "let",
            Keyword::Fun => "fun",
            Keyword::Return => "return",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
        }
    }

    pub(crate) fn get_all() -> &'static [Keyword] {
        &[
            Keyword::Let,
            Keyword::Fun,
            Keyword::Return,
            Keyword::If,
            Keyword::Else,
            Keyword::While,
        ]
    }
}
