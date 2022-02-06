use logos::Logos;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Display};
use text_position_rs::CompositePosition;

#[derive(Logos, Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum Token {
    // Trivias:
    #[error]
    Bad,
    #[regex("[ \t]+")]
    Blank,
    #[regex("(\r?\n[ \t]*)+")]
    Newlines,
    #[regex("//[^\r\n]*")]
    Comment,

    // Literals:
    #[regex("[0-9]+")]
    DecimalInt,
    #[regex(r#""(\\x[0-9A-Fa-f][0-9A-Fa-f]|\\.|[^\\"\r\n])*""#)]
    String,

    // Identifiers:
    #[token("false")]
    False,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("use")]
    Use,
    #[regex(r#"[A-Za-z_]([0-9A-Za-z_]|[^\x00-\x7f])*"#)]
    Ident,

    // Punctuation:
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("&=")]
    AndEqual,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token("#")]
    Hash,
    #[token("^")]
    Hat,
    #[token("^=")]
    HatEqual,
    #[token("<")]
    Left,
    #[token("<=")]
    LeftEqual,
    #[token("<=>")]
    LeftEqualRight,
    #[token("<<")]
    LeftLeft,
    #[token("<<=")]
    LeftLeftEqual,
    #[token("<-")]
    LeftSlimArrow,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEqual,
    #[token("%")]
    Percent,
    #[token("%=")]
    PercentEqual,
    #[token("|")]
    Pipe,
    #[token("|=")]
    PipeEqual,
    #[token("||")]
    PipePipe,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusEqual,
    #[token("?")]
    Question,
    #[token(">")]
    Right,
    #[token(">=")]
    RightEqual,
    #[token("=>")]
    RightFatArrow,
    #[token("->")]
    RightSlimArrow,
    #[token(">>")]
    RightRight,
    #[token(">>=")]
    RightRightEqual,
    #[token(";")]
    Semi,
    #[token("/")]
    Slash,
    #[token("/=")]
    SlashEqual,
    #[token("*")]
    Star,
    #[token("*=")]
    StarEqual,
}

impl Token {
    pub fn is_trailing_trivia(self) -> bool {
        match self {
            Token::Bad | Token::Blank | Token::Comment => true,
            _ => false,
        }
    }

    pub fn is_trivia(self) -> bool {
        self == Token::Newlines || self.is_trailing_trivia()
    }

    pub fn at_statement_last(self) -> bool {
        match self {
            Token::DecimalInt
            | Token::String
            | Token::False
            | Token::Return
            | Token::True
            | Token::Ident
            | Token::RightParen
            | Token::RightBracket
            | Token::RightBrace => true,
            _ => false,
        }
    }

    pub fn is_semi(self) -> bool {
        self == Token::Semi
    }
}

pub fn tokenize_to_vec(text: &str) -> Vec<(Token, u32)> {
    let lexer = logos::Lexer::new(text);
    lexer
        .spanned()
        .map(|(token, range)| (token, range_to_len(range) as u32))
        .collect()
}

pub struct Tokenizer<'b> {
    pub(crate) source_code: &'b str,
    lexer: logos::Lexer<'b, Token>,
    pos: CompositePosition,
    last: Option<(Token, usize)>,
}

impl<'b> Tokenizer<'b> {
    pub fn new(source_code: &'b str) -> Self {
        let mut lexer = logos::Lexer::new(source_code);
        let last = lexer
            .next()
            .map(|token| (token, range_to_len(lexer.span())));

        Self {
            source_code,
            lexer,
            pos: CompositePosition::new(0, 0, 0, 0),
            last,
        }
    }

    fn end_pos(&self, pos: CompositePosition, len: usize) -> CompositePosition {
        let i = pos.index as usize;
        let s = &self.source_code[i..i + len];
        pos + CompositePosition::from(s)
    }

    fn bump(&mut self) {
        let last = self
            .lexer
            .next()
            .map(|token| (token, range_to_len(self.lexer.span())));
        self.last = last;
    }

    pub fn next(&mut self) -> Option<(Token, &'b str, CompositePosition)> {
        let start = self.pos;

        let mut leading_len = 0_usize;
        while let Some(len) = self
            .last
            .and_then(|(t, len)| if t.is_trivia() { Some(len) } else { None })
        {
            leading_len += len;
            self.bump();
        }

        let (main_token, main_len) = match self.last {
            Some(it) => it,
            None => {
                return None;
            }
        };
        let main_pos = self.end_pos(start, leading_len);
        self.bump();

        let mut trailing_len = 0_usize;
        while let Some(len) = self.last.and_then(|(t, len)| {
            if t.is_trailing_trivia() {
                Some(len)
            } else {
                None
            }
        }) {
            trailing_len += len;
            self.bump();
        }

        let end_pos = self.end_pos(main_pos, main_len + trailing_len);
        self.pos = end_pos;

        let main_text = {
            let i = main_pos.index as usize;
            &self.source_code[i..i + main_len]
        };

        Some((main_token, main_text, main_pos))
    }
}

fn range_to_len(range: std::ops::Range<usize>) -> usize {
    range.end - range.start
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}
