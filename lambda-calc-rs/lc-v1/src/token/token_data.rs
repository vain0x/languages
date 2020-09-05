use super::token_kind::TokenKind;
use std::fmt::{self, Debug};

pub(crate) struct TokenData {
    pub(crate) text: String,
    pub(crate) kind: TokenKind,
    pub(crate) len: usize,
}

impl Debug for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::NewLine
            | TokenKind::Blank
            | TokenKind::Comment
            | TokenKind::BadToken
            | TokenKind::Number
            | TokenKind::Ident => Debug::fmt(&self.text, f),
            _ => Debug::fmt(&self.kind, f),
        }
    }
}
