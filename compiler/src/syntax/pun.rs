/// Something with static text representation:
/// punctuation, operator or keyword.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Pun {
    // Punctuation:
    ParenL,
    ParenR,
    BracketL,
    BracketR,
    BraceL,
    BraceR,
    Comma,
    Semi,

    // Operator:
    /// `=`
    Set,
    /// `+=`
    SetAdd,
    SetSub,
    SetMul,
    SetDiv,
    SetMod,
    Range,
    /// `||`
    LogOr,
    /// `&&`
    LogAnd,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    Add,
    Sub,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    Mul,
    Div,
    Mod,
    /// `&`
    BitAnd,
    /// `<<`
    BitShiftL,
    /// `>>`
    BitShiftR,
    /// `:`
    Anno,

    // Keyword:
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
    /// `mod`
    Module,
    Pub,
    Use,
    Extern,
}

const PUNS: &[(Pun, &str)] = &[
    (Pun::ParenL, "("),
    (Pun::ParenR, ")"),
    (Pun::BracketL, "["),
    (Pun::BracketR, "]"),
    (Pun::BraceL, "{"),
    (Pun::BraceR, "}"),
    (Pun::Comma, ","),
    (Pun::Semi, ";"),
    (Pun::Set, "="),
    (Pun::SetAdd, "+="),
    (Pun::SetSub, "-="),
    (Pun::SetMul, "*="),
    (Pun::SetDiv, "/="),
    (Pun::SetMod, "%="),
    (Pun::Range, ".."),
    (Pun::LogOr, "||"),
    (Pun::LogAnd, "&&"),
    (Pun::Eq, "=="),
    (Pun::Ne, "!="),
    (Pun::Lt, "<"),
    (Pun::Le, "<="),
    (Pun::Gt, ">"),
    (Pun::Ge, ">="),
    (Pun::Add, "+"),
    (Pun::Sub, "-"),
    (Pun::BitOr, "|"),
    (Pun::BitXor, "^"),
    (Pun::Mul, "*"),
    (Pun::Div, "/"),
    (Pun::Mod, "%"),
    (Pun::BitAnd, "&"),
    (Pun::BitShiftL, "<<"),
    (Pun::BitShiftR, ">>"),
    (Pun::Anno, ":"),
    (Pun::Do, "do"),
    (Pun::As, "as"),
    (Pun::Let, "let"),
    (Pun::Rec, "rec"),
    (Pun::Fn, "fn"),
    (Pun::Return, "return"),
    (Pun::If, "if"),
    (Pun::Else, "else"),
    (Pun::While, "while"),
    (Pun::Break, "break"),
    (Pun::Continue, "continue"),
    (Pun::For, "for"),
    (Pun::In, "in"),
    (Pun::Type, "type"),
    (Pun::Struct, "struct"),
    (Pun::Enum, "enum"),
    (Pun::Trait, "trait"),
    (Pun::Impl, "impl"),
    (Pun::Module, "mod"),
    (Pun::Pub, "pub"),
    (Pun::Use, "use"),
    (Pun::Extern, "extern"),
];

impl Pun {
    pub(crate) fn get_all() -> Vec<Pun> {
        PUNS.iter().map(|&(pun, _)| pun).collect()
    }

    pub(crate) fn text(self) -> &'static str {
        PUNS.iter()
            .filter_map(|&(pun, text)| if pun == self { Some(text) } else { None })
            .next()
            .unwrap_or_else(|| panic!("Unknown pun {:?}", self))
    }

    pub(crate) fn parse(text: &str) -> Option<Pun> {
        PUNS.iter()
            .filter_map(|&(pun, pun_text)| if pun_text == text { Some(pun) } else { None })
            .next()
    }

    fn is_identifier(self) -> bool {
        self.text().bytes().all(ctype::is_ident_char)
    }

    pub(crate) fn keywords() -> Vec<Pun> {
        Pun::get_all()
            .into_iter()
            .filter(|pun| pun.is_identifier())
            .collect()
    }
}

pub(crate) mod ctype {
    pub(crate) fn is_ascii_digit(c: u8) -> bool {
        b'0' <= c && c <= b'9'
    }

    pub(crate) fn is_ident_char(c: u8) -> bool {
        (b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z' || is_ascii_digit(c) || c == b'_')
    }

    pub(crate) fn is_op_char(c: u8) -> bool {
        b"!*+-./%<=>?@^~&|:".contains(&c)
    }

    pub(crate) fn is_whitespace(c: u8) -> bool {
        c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
    }
}
