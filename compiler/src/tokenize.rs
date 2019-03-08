use crate::*;

#[derive(Default)]
struct Tokenizer<'a> {
    src: &'a str,
    current: usize,
    tokens: BTreeMap<TokenId, Token>,
}

impl Tokenizer<'_> {
    fn add_token(&mut self, kind: TokenKind, span: Span) {
        let token_id = TokenId(self.tokens.len());
        self.tokens.insert(token_id, Token { kind, span });
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
        Some((self.src[l..r].to_string(), (l, r)))
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
        self.add_token(TokenKind::Str, (l, r + 1));
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
            if let Some((_, span)) = self.read_while(is_ascii_digit) {
                self.add_token(TokenKind::Int, span);
                continue;
            }
            if let Some((word, span)) = self.read_while(is_ident_char) {
                if let Some(&keyword) = Keyword::get_all()
                    .iter()
                    .find(|&&keyword| keyword.text() == &word)
                {
                    self.add_token(TokenKind::Keyword(keyword), span);
                } else {
                    self.add_token(TokenKind::Ident, span);
                }
                continue;
            }
            if let Some((word, span)) = self.read_while(is_op_char) {
                if let Some(&(_, op, _)) = OPS.iter().find(|&&(op_text, _, _)| op_text == &word) {
                    self.add_token(TokenKind::Op(op), span);
                } else {
                    self.add_token(TokenKind::Err, (l, self.current));
                }
                continue;
            }
            if self.next_char() == b'"' {
                self.read_str();
                continue;
            }
            for pun in PUNS {
                if self.reads(pun) {
                    self.add_token(TokenKind::Pun(pun), (l, self.current));
                    continue 't;
                }
            }

            let char_len = self.src[self.current..]
                .char_indices()
                .next()
                .map(|(n, _)| n)
                .unwrap_or(0);
            self.current += char_len;
            self.add_token(TokenKind::Err, (l, self.current));
        }
        self.add_token(TokenKind::Eof, (self.current, self.current));
    }
}

fn is_ascii_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_ident_char(c: u8) -> bool {
    (b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z' || is_ascii_digit(c) || c == b'_')
}

fn is_op_char(c: u8) -> bool {
    b"!*+-./%<=>?@^~".contains(&c)
}

fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

pub(crate) fn tokenize(src: &str) -> BTreeMap<TokenId, Token> {
    let mut tokenizer = Tokenizer {
        src,
        ..Tokenizer::default()
    };
    tokenizer.tokenize();
    tokenizer.tokens
}
