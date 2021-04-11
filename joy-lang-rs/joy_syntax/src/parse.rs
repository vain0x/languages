use crate::tokenize::{JoyTokenizer, Token};
use bumpalo::Bump;
use std::fmt::{self, Debug};

use text_position_rs::CompositePosition;
type Utf8Pos = text_position_rs::Utf8Position;

#[derive(Copy, Clone, PartialEq)]
pub struct Pos(CompositePosition);

impl Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Utf8Pos::from(self.0).fmt(f)
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos(CompositePosition::new(0, 0, 0, 0))
    }
}

impl From<CompositePosition> for Pos {
    fn from(pos: CompositePosition) -> Self {
        Pos(pos)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Copy, Debug, Default)]
pub struct LexicalError;

/// For lalrpop.
pub struct MyLexer<'b> {
    tokenizer: JoyTokenizer<'b>,
}

impl<'b> Iterator for MyLexer<'b> {
    type Item = Spanned<Token, Pos, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, text, pos) = self.tokenizer.next()?;
        Some(Ok((
            pos.into(),
            token,
            (pos + CompositePosition::from(text)).into(),
        )))
    }
}

#[test]
fn parse_test() {
    use crate::grammar::*;

    let mut lexer = MyLexer {
        tokenizer: JoyTokenizer::new(
            r#"(
            + 42 )"#,
        ),
    };

    RootParser::new().parse(&mut lexer).unwrap();
    // panic!();
}
