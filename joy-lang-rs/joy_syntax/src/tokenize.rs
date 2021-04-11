use logos::Logos;
use text_position_rs::CompositePosition;

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
pub enum Token {
    // Trivias:
    #[error]
    Bad,
    #[regex("[ \t\u{3000}]+")]
    Blank,
    #[regex("(\r?\n[ \t\u{3000}]*)+")]
    Newlines,
    #[regex("//[^\r\n]*")]
    Comment,

    // Extra:
    Eos,

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
    #[token("&&&")]
    AndAndAnd,
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
    #[token("..=")]
    DotDotEqual,
    #[token("..<")]
    DotDotLeft,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token("#")]
    Hash,
    #[token("#!")]
    HashBang,
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
    #[token("--")]
    MinusMinus,
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
    #[token("++")]
    PlusPlus,
    #[token("?")]
    Question,
    #[token("??")]
    QuestionQuestion,
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
    #[token("**")]
    StarStar,
    #[token("**=")]
    StarStarEqual,
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

/// For parser.
pub struct JoyTokenizer<'b> {
    pub(crate) source_code: &'b str,
    lexer: logos::Lexer<'b, Token>,
    pos: CompositePosition,
    last: Option<(Token, usize)>,
    auto_eos: bool,
    fused: bool,
}

impl<'b> JoyTokenizer<'b> {
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
            auto_eos: false,
            fused: last.is_none(),
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
        if self.fused {
            return None;
        }

        if self.auto_eos {
            self.auto_eos = false;
            return Some((Token::Eos, "", self.pos));
        }

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
                self.fused = true;
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

        if main_token.is_semi() {
            return Some((Token::Eos, "", main_pos));
        }

        let main_text = {
            let i = main_pos.index as usize;
            &self.source_code[i..i + main_len]
        };

        if main_token.at_statement_last() && self.last.map_or(true, |(t, _)| t == Token::Newlines) {
            self.auto_eos = true;
        }

        Some((main_token, main_text, main_pos))
    }
}

fn range_to_len(range: std::ops::Range<usize>) -> usize {
    range.end - range.start
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn lex_test(input: &str, expect: Expect) {
        let mut lex = Token::lexer(input);
        let mut output = String::new();
        while let Some(token) = lex.next() {
            let span = lex.span();
            output += &format!("{}..{}  ", span.start, span.end);

            match token {
                Token::Blank
                | Token::Newlines
                | Token::Comment
                | Token::DecimalInt
                | Token::String
                | Token::Ident => output += &format!("{:?} {:?}\n", token, &input[span]),
                _ => {
                    output += &format!("{:?}\n", token);
                }
            }
        }
        expect.assert_eq(&output);
    }

    #[test]
    fn basic_lex_test() {
        lex_test(
            r#"
                // The function.
                fn main() {
                    printfn("Hello, world!")
                }
            "#,
            expect![[r#"
                0..17  Newlines "\n                "
                17..33  Comment "// The function."
                33..50  Newlines "\n                "
                50..52  Fn
                52..53  Blank " "
                53..57  Ident "main"
                57..58  LeftParen
                58..59  RightParen
                59..60  Blank " "
                60..61  LeftBrace
                61..82  Newlines "\n                    "
                82..89  Ident "printfn"
                89..90  LeftParen
                90..105  String "\"Hello, world!\""
                105..106  RightParen
                106..123  Newlines "\n                "
                123..124  RightBrace
                124..137  Newlines "\n            "
            "#]],
        );
    }

    #[test]
    fn escape_sequence_lex_test() {
        lex_test(
            r#"
                "\\"
                "\"\r\n\""
                "\x7f"
            "#,
            expect![[r#"
                0..17  Newlines "\n                "
                17..21  String "\"\\\\\""
                21..38  Newlines "\n                "
                38..48  String "\"\\\"\\r\\n\\\"\""
                48..65  Newlines "\n                "
                65..71  String "\"\\x7f\""
                71..84  Newlines "\n            "
            "#]],
        );
    }

    #[test]
    fn exotic_lex_test() {
        lex_test(
            r#"Non-ascii identifier.
                _ぺんぎんfn
                42_with_suffix
                "String literal doesn't end with newline.
                // Line comment ends without newline."#,
            expect![[r#"
                0..3  Ident "Non"
                3..4  Minus
                4..9  Ident "ascii"
                9..10  Blank " "
                10..20  Ident "identifier"
                20..21  Dot
                21..38  Newlines "\n                "
                38..53  Ident "_ぺんぎんfn"
                53..70  Newlines "\n                "
                70..72  DecimalInt "42"
                72..84  Ident "_with_suffix"
                84..101  Newlines "\n                "
                101..142  Bad
                142..159  Newlines "\n                "
                159..196  Comment "// Line comment ends without newline."
            "#]],
        );
    }
}
