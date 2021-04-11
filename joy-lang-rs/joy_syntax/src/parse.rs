use crate::{
    ast::ARoot,
    tokenize::{JoyTokenizer, Token},
};
use bumpalo::Bump;
use std::fmt::{self, Debug, Display};

use text_position_rs::CompositePosition;
type Utf8Pos = text_position_rs::Utf8Position;

#[derive(Copy, Clone, PartialEq)]
pub struct Pos(CompositePosition);

impl Pos {
    pub fn index(&self) -> usize {
        self.0.index as usize
    }
}

impl Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&Utf8Pos::from(self.0), f)
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&Utf8Pos::from(self.0), f)
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

impl Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Lexical error")
    }
}

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

#[derive(Debug)]
pub struct ParseError<'b> {
    pub msg: &'b str,
    pub pos: Pos,
}

pub fn parse_from_string<'b>(
    source_code: &'b str,
    bump: &'b Bump,
) -> Result<ARoot<'b>, ParseError<'b>> {
    let mut lexer = MyLexer {
        tokenizer: JoyTokenizer::new(source_code),
    };

    match crate::grammar::RootParser::new().parse(source_code, &bump, &mut lexer) {
        Ok(it) => Ok(it),
        Err(err) => {
            let pos = match err {
                lalrpop_util::ParseError::InvalidToken { location: l }
                | lalrpop_util::ParseError::UnrecognizedEOF { location: l, .. }
                | lalrpop_util::ParseError::UnrecognizedToken {
                    token: (l, _, _), ..
                }
                | lalrpop_util::ParseError::ExtraToken { token: (l, _, _) } => l,
                lalrpop_util::ParseError::User { .. } => Pos::default(),
            };
            Err(ParseError {
                msg: bump.alloc_str(&format!("{}", err)),
                pos,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect_file, ExpectFile};

    fn should_parse(source_code: &str, expect: ExpectFile) {
        let bump = Bump::new();
        let result = parse_from_string(source_code, &bump).unwrap();
        expect.assert_debug_eq(&result);
    }

    fn should_fail(source_code: &str, expect: ExpectFile) {
        let bump = Bump::new();
        let result = parse_from_string(source_code, &bump).expect_err("should fail");
        expect.assert_eq(&format!("Parse error: {} at {:?}", result.msg, result.pos));
    }

    macro_rules! should_parse {
        ($($name:ident ,)*) => {
            $(
                #[test]
                fn $name() {
                    should_parse(
                        include_str!(concat!("../../tests/parse/",  stringify!($name), ".joy")),
                        expect_file![concat!("../../tests/parse/",  stringify!($name), ".generated.txt")],
                    );
                }
            )*
        }
    }

    macro_rules! should_fail {
        ($($name:ident ,)*) => {
            $(
                #[test]
                fn $name() {
                    should_fail(
                        include_str!(concat!("../../tests/parse/",  stringify!($name), ".joy")),
                        expect_file![concat!("../../tests/parse/",  stringify!($name), ".generated.txt")],
                    );
                }
            )*
        }
    }

    should_parse! {
        ok_empty,
        ok_zero,
        ok_multiline_expr,
        ok_let,
        ok_fn,
        ok_call_expr,
        ok_semi,
    }

    should_fail! {
        err_broken_arithmetic,
        err_two_ints,
    }
}
