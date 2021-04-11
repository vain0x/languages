use crate::tokenize::{JoyTokenizer, Token};
use bumpalo::Bump;

type Pos = text_position_rs::CompositePosition;
type Utf8Pos = text_position_rs::Utf8Position;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Copy, Debug, Default)]
pub struct LexicalError;

/// For lalrpop.
pub struct MyLexer<'b> {
    tokenizer: JoyTokenizer<'b>,
}

impl<'b> Iterator for MyLexer<'b> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, text, pos) = self.tokenizer.next()?;
        Some(Ok((
            pos.index as usize,
            token,
            pos.index as usize + text.len(),
        )))
    }
}

#[test]
fn parse_test() {
    use crate::grammar::*;

    let mut lexer = MyLexer {
        tokenizer: JoyTokenizer::new("(42)"),
    };

    TermParser::new().parse(&mut lexer).unwrap();
}
