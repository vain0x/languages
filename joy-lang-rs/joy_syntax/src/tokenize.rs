use logos::{Logos};

#[derive(Logos, Debug, PartialEq)]
enum Token {
    // Trivias:
    #[error]
    Bad,
    #[regex("[ \t\u{3000}]+")]
    Blank,
    #[regex("(\r?\n[ \t\u{3000}]*)+")]
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
                    printfn "Hello, world!"
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
                89..90  Blank " "
                90..105  String "\"Hello, world!\""
                105..122  Newlines "\n                "
                122..123  RightBrace
                123..136  Newlines "\n            "
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
