use crate::*;

#[derive(Default)]
struct Tokenizer<'a> {
    src: &'a str,
    current: usize,
    tokens: Vec<Token>,
    token_spans: Vec<Span>,
}

impl Tokenizer<'_> {
    fn add_token(&mut self, token: Token, span: Span) {
        self.tokens.push(token);
        self.token_spans.push(span);
    }

    fn next_char(&self) -> u8 {
        if self.at_eof() {
            return 0;
        }
        self.src.as_bytes()[self.current]
    }

    fn at_eof(&self) -> bool {
        self.current >= self.src.len()
    }

    fn read_while<P: Fn(u8) -> bool>(&mut self, pred: P) -> Option<(String, Span)> {
        let l = self.current;
        if !pred(self.next_char()) {
            return None;
        }
        while !self.at_eof() && pred(self.next_char()) {
            self.current += 1;
        }
        let r = self.current;
        Some((self.src[l..r].into(), (l, r)))
    }

    fn reads(&mut self, prefix: &str) -> bool {
        if self.src[self.current..].starts_with(prefix) {
            self.current += prefix.len();
            return true;
        }
        false
    }

    fn read_str(&mut self) {
        assert_eq!(self.next_char(), b'"');

        let l = self.current;
        self.current += 1;
        let p = |c: u8| c != b'"' && c != b'\n';
        while p(self.next_char()) {
            self.current += 1;
        }
        let r = self.current;
        self.current += 1;
        let word = self.src[l + 1..r].into();
        self.add_token(Token::Str(word), (l, r + 1));
    }

    pub fn tokenize(&mut self) {
        't: while !self.at_eof() {
            let l = self.current;
            if self.reads("//") {
                self.read_while(|c| c != b'\n');
                continue;
            }
            if let Some(_) = self.read_while(is_whitespace) {
                continue;
            }
            if let Some((word, span)) = self.read_while(is_ascii_digit) {
                self.add_token(Token::Int(word.parse().unwrap_or(0)), span);
                continue;
            }
            if let Some((word, span)) = self.read_while(is_id_char) {
                self.add_token(Token::Id(word.into()), span);
                continue;
            }
            if self.next_char() == b'"' {
                self.read_str();
                continue;
            }
            for pun in PUNS {
                if self.reads(pun) {
                    self.add_token(Token::Pun(pun), (l, self.current));
                    continue 't;
                }
            }
            self.current += 1;
            self.add_token(Token::Err("?".into()), (l, self.current));
        }
        self.add_token(Token::Eof, (self.current, self.current));
    }
}

fn is_ascii_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_id_char(c: u8) -> bool {
    (b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z' || is_ascii_digit(c))
        || b"!#$'*+-./%<=>?@^_~".contains(&c)
}

fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

pub fn tokenize(src: &str) -> (Vec<Token>, Vec<Span>) {
    let mut tokenizer = Tokenizer {
        src,
        ..Tokenizer::default()
    };
    tokenizer.tokenize();
    (tokenizer.tokens, tokenizer.token_spans)
}
