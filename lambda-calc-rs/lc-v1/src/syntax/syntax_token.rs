use crate::token::token_kind::TokenKind;
use std::fmt::{self, Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) struct SyntaxToken<'a> {
    pub(crate) index: usize,
    pub(crate) kind: TokenKind,
    pub(crate) text: &'a str,
}

impl Debug for SyntaxToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
