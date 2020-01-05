//! 中間言語のトークン

use al_aux::syntax::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum IlTokenKind {
    Error,
    Eof,
    Trivia,
    Int,

    /// `$foo`
    Ident,
    Atom,
    ParenL,
    ParenR,
}

#[derive(Clone, Debug)]
pub(crate) struct IlToken {
    kind: IlTokenKind,
    start: usize,
    end: usize,
}

pub(crate) struct IlTokenFactory;

impl IlToken {
    pub(crate) fn kind(&self) -> IlTokenKind {
        self.kind
    }

    pub(crate) fn start(&self) -> usize {
        self.start
    }

    pub(crate) fn end(&self) -> usize {
        self.end
    }
}

impl TokenKindTrait for IlTokenKind {
    fn error() -> Self {
        IlTokenKind::Error
    }

    fn eof() -> Self {
        IlTokenKind::Eof
    }

    fn is_trivia(&self) -> bool {
        *self == IlTokenKind::Trivia
    }
}

impl TokenTrait for IlToken {
    type Kind = IlTokenKind;

    fn kind(&self) -> IlTokenKind {
        self.kind()
    }
}

impl TokenFactoryTrait for IlTokenFactory {
    type Token = IlToken;

    type Kind = IlTokenKind;

    fn new_token(&self, kind: Self::Kind, start: usize, end: usize) -> Self::Token {
        IlToken { kind, start, end }
    }
}
