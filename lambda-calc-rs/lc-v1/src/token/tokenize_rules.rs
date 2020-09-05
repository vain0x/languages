use super::{token_data::TokenData, token_kind::TokenKind};
use lc_utils::{
    deque_chan::DequeSender,
    tokenizer::{Tokenizer, TokenizerHost},
};

type Tx<'a, 'h> = Tokenizer<'a, 'h, MyTokenizerHost<'h>>;

pub(crate) struct MyTokenizerHost<'h> {
    pub(crate) tokens: DequeSender<'h, TokenData>,
}

impl<'h> MyTokenizerHost<'h> {
    pub(crate) fn new(tokens: DequeSender<'h, TokenData>) -> Self {
        Self { tokens }
    }
}

impl<'h> TokenizerHost for MyTokenizerHost<'h> {
    type Kind = TokenKind;

    fn on_token(&mut self, text: &str, kind: TokenKind, start: usize, end: usize) {
        self.tokens.push_back(TokenData {
            text: text.to_string(),
            kind,
            len: end - start,
        });
    }
}

enum Lookahead {
    Eof,
    CrLf,
    Cr,
    Lf,
    Blank,
    Comment,
    BadToken,
    Number,
    Ident,
    Token(TokenKind, usize),
}

fn lookahead(tx: &mut Tx) -> Lookahead {
    match tx.next() {
        '\0' => Lookahead::Eof,
        '\r' => match tx.nth(1) {
            '\n' => Lookahead::CrLf,
            _ => Lookahead::Cr,
        },
        '\n' => Lookahead::Lf,
        ' ' | '\t' => Lookahead::Blank,
        '0'..='9' => Lookahead::Number,
        'A'..='Z' | 'a'..='z' | '_' => Lookahead::Ident,
        '(' => Lookahead::Token(TokenKind::OpenParen, 1),
        ')' => Lookahead::Token(TokenKind::CloseParen, 1),
        ',' => Lookahead::Token(TokenKind::Comma, 1),
        '=' => Lookahead::Token(TokenKind::Equal, 1),
        ';' => Lookahead::Token(TokenKind::SemiColon, 1),
        '/' if tx.nth(1) == '/' => Lookahead::Comment,
        _ => Lookahead::BadToken,
    }
}

fn scan_newline(leading: usize, tx: &mut Tx) -> TokenKind {
    tx.bump_bytes(leading);

    while tx.next().is_whitespace() {
        tx.bump();
    }

    TokenKind::NewLine
}

fn scan_blank(tx: &mut Tx) -> TokenKind {
    loop {
        match tx.next() {
            '\r' | '\n' => break,
            c if c.is_whitespace() => tx.bump(),
            _ => break,
        }
    }

    TokenKind::Blank
}

fn scan_bad_token(tx: &mut Tx) -> TokenKind {
    // 次の ASCII 文字まで飛ばす。(次が ASCII 文字ならそれを飛ばす。)
    let len = tx
        .following_text()
        .bytes()
        .take_while(|&b| {
            let c = b as char;
            !c.is_ascii_graphic() && !c.is_ascii_whitespace()
        })
        .count()
        .max(1);

    tx.bump_bytes(len);

    TokenKind::BadToken
}

fn scan_number(tx: &mut Tx) -> TokenKind {
    while tx.next().is_ascii_digit() {
        tx.bump();
    }

    TokenKind::Number
}

fn scan_ident(tx: &mut Tx) -> TokenKind {
    while let '0'..='9' | 'A'..='Z' | 'a'..='z' | '_' = tx.next() {
        tx.bump();
    }

    TokenKind::from_ident(tx.current_text())
}

pub(crate) fn tokenize_advance(tx: &mut Tx) -> bool {
    let kind = match lookahead(tx) {
        Lookahead::Eof => {
            tx.commit(TokenKind::Eof);
            return false;
        }
        Lookahead::CrLf => scan_newline(2, tx),
        Lookahead::Cr | Lookahead::Lf => scan_newline(1, tx),
        Lookahead::Blank => scan_blank(tx),
        Lookahead::Comment => {
            tx.bump_line();
            TokenKind::Comment
        }
        Lookahead::BadToken => scan_bad_token(tx),
        Lookahead::Number => scan_number(tx),
        Lookahead::Ident => scan_ident(tx),
        Lookahead::Token(kind, len) => {
            tx.bump_bytes(len);
            kind
        }
    };
    tx.commit(kind);
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use lc_utils::deque_chan::deque_chan;
    use std::collections::VecDeque;

    fn tokenize(source_code: &str) -> Vec<TokenData> {
        let mut deque = VecDeque::new();
        let (tx, _) = deque_chan(&mut deque);
        let mut host = MyTokenizerHost::new(tx);

        {
            let mut tx = Tx::new(source_code, &mut host);
            while tokenize_advance(&mut tx) {}
            tx.finish();
        }

        deque.into_iter().collect()
    }

    fn do_test_tokenize(source_code: &str, expect: Expect) {
        let actual = format!("{:#?}", tokenize(source_code));
        expect.assert_eq(&actual);
    }

    #[test]
    fn test_comment() {
        do_test_tokenize(
            r#"
                // Define an identity function! 🐧
                let id =
                    // 🐧
                    fn(x)
                    // 🐧;;
                    x;
            "#
            .trim(),
            expect![[r#"
                [
                    "// Define an identity function! 🐧",
                    "\n                ",
                    Let,
                    " ",
                    "id",
                    " ",
                    Equal,
                    "\n                    ",
                    "// 🐧",
                    "\n                    ",
                    Fn,
                    OpenParen,
                    "x",
                    CloseParen,
                    "\n                    ",
                    "// 🐧;;",
                    "\n                    ",
                    "x",
                    SemiColon,
                    Eof,
                ]"#]],
        );
    }

    #[test]
    fn test_let() {
        do_test_tokenize(
            "let id = fn(x) x;",
            expect![[r#"
                [
                    Let,
                    " ",
                    "id",
                    " ",
                    Equal,
                    " ",
                    Fn,
                    OpenParen,
                    "x",
                    CloseParen,
                    " ",
                    "x",
                    SemiColon,
                    Eof,
                ]"#]],
        );
    }
}
