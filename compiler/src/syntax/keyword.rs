#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Keyword {
    Let,
    Return,
    If,
    Else,
    While,
    Break,
    Continue,

    // Reserved:
    Do,
    As,
    Rec,
    Fn,
    For,
    In,
    Type,
    Struct,
    Enum,
    Trait,
    Impl,
    Mod,
    Pub,
    Use,
    Extern,
}

impl Keyword {
    pub(crate) fn text(self) -> &'static str {
        match self {
            Keyword::Do => "do",
            Keyword::As => "as",
            Keyword::Let => "let",
            Keyword::Rec => "rec",
            Keyword::Fn => "fn",
            Keyword::Return => "return",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::For => "for",
            Keyword::In => "in",
            Keyword::Type => "type",
            Keyword::Struct => "struct",
            Keyword::Enum => "enum",
            Keyword::Trait => "trait",
            Keyword::Impl => "impl",
            Keyword::Mod => "mod",
            Keyword::Pub => "pub",
            Keyword::Use => "use",
            Keyword::Extern => "extern",
        }
    }

    pub(crate) fn get_all() -> &'static [Keyword] {
        &[
            Keyword::Do,
            Keyword::As,
            Keyword::Let,
            Keyword::Rec,
            Keyword::Fn,
            Keyword::Return,
            Keyword::If,
            Keyword::Else,
            Keyword::While,
            Keyword::Break,
            Keyword::Continue,
            Keyword::For,
            Keyword::In,
            Keyword::Type,
            Keyword::Struct,
            Keyword::Enum,
            Keyword::Trait,
            Keyword::Impl,
            Keyword::Mod,
            Keyword::Pub,
            Keyword::Use,
            Keyword::Extern,
        ]
    }
}
