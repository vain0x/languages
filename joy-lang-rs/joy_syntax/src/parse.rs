pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum Token {
    LeftParen,
    RightParen,
    Number,
}

pub(crate) type LexicalError = ();

struct MyLexer {
    index: usize,
    tokens: Vec<(Token, usize)>,
}

impl Iterator for MyLexer {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.index;
        let (token, len) = self.tokens.pop()?;
        self.index += len;
        Some(Ok((i, token, self.index)))
    }
}

#[test]
fn calculator1() {
    use crate::grammar::*;

    let mut tokens = vec![
        (Token::LeftParen, 1),
        (Token::Number, 2),
        (Token::RightParen, 1),
    ];
    tokens.reverse();

    TermParser::new()
        .parse(MyLexer { index: 0, tokens })
        .unwrap();
}
