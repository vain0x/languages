#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Keyword {
    Let,
    Return,
    If,
    Else,
    While,
    Break,
    Continue,
}

impl Keyword {
    pub(crate) fn text(self) -> &'static str {
        match self {
            Keyword::Let => "let",
            Keyword::Return => "return",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
        }
    }

    pub(crate) fn get_all() -> &'static [Keyword] {
        &[
            Keyword::Let,
            Keyword::Return,
            Keyword::If,
            Keyword::Else,
            Keyword::While,
            Keyword::Break,
            Keyword::Continue,
        ]
    }
}
