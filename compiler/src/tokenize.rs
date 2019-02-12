use crate::*;

#[derive(Default)]
struct Tokenizer {
    src: String,
    current: usize,
    tokens: Vec<Token>,
    token_spans: Vec<Span>,
}

impl Tokenizer {
    fn push(&mut self, token: Token, span: Span) {
        self.tokens.push(token);
        self.token_spans.push(span);
    }

    fn c(&self) -> u8 {
        if self.current >= self.src.as_bytes().len() {
            return 0;
        }
        self.src.as_bytes()[self.current]
    }

    fn take<P: Fn(u8) -> bool>(&mut self, pred: P) -> Option<(String, Span)> {
        let l = self.current;
        if !pred(self.c()) {
            return None;
        }
        while pred(self.c()) {
            self.current += 1;
        }
        let r = self.current;
        Some((self.src[l..r].into(), (l, r)))
    }

    fn expect(&mut self, prefix: &str) -> bool {
        if self.src[self.current..].starts_with(prefix) {
            self.current += prefix.len();
            return true;
        }
        false
    }

    pub fn tokenize(&mut self) {
        't: while self.current < self.src.len() {
            let l = self.current;
            if self.expect("//") {
                self.take(|c| c != b'\n');
                continue;
            }
            if let Some(_) = self.take(is_whitespace) {
                continue;
            }
            if let Some((word, span)) = self.take(is_ascii_digit) {
                self.push(Token::Int(word.parse().unwrap_or(0)), span);
                continue;
            }
            if let Some((word, span)) = self.take(is_id_char) {
                self.push(Token::Id(word.into()), span);
                continue;
            }
            if self.c() == b'"' {
                self.current += 1;
                let p = |c: u8| c != b'"' && c != b'\n' && c != 0;
                while p(self.c()) {
                    self.current += 1;
                }
                let r = self.current;
                self.current += 1;
                let word = self.src[l + 1..r].into();
                self.push(Token::Str(word), (l, r + 1));
                continue;
            }
            for pun in PUNS {
                if self.expect(pun) {
                    self.push(Token::Pun(pun), (l, self.current));
                    continue 't;
                }
            }
            self.current += 1;
            self.push(Token::Err("?".into()), (l, self.current));
        }
        self.push(Token::Eof, (self.current, self.current));
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

pub fn tokenize(src: String) -> (Vec<Token>, Vec<Span>) {
    let mut tokenizer = Tokenizer {
        src,
        ..Tokenizer::default()
    };
    tokenizer.tokenize();
    (tokenizer.tokens, tokenizer.token_spans)
}
