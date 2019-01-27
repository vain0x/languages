use crate::*;

#[derive(Default)]
pub struct Tokenizer {
    pub src: String,
    pub cur: usize,
    pub toks: Toks,
}

impl Tokenizer {
    fn c(&self) -> u8 {
        if self.cur >= self.src.as_bytes().len() {
            return 0;
        }
        self.src.as_bytes()[self.cur]
    }

    fn take<P: Fn(u8) -> bool>(&mut self, pred: P) -> Option<(String, Range)> {
        let l = self.cur;
        if !pred(self.c()) {
            return None;
        }
        while pred(self.c()) {
            self.cur += 1;
        }
        let r = self.cur;
        Some((self.src[l..r].into(), (l, r)))
    }

    fn expect(&mut self, prefix: &str) -> bool {
        if self.src[self.cur..].starts_with(prefix) {
            self.cur += prefix.len();
            return true;
        }
        false
    }

    pub fn tokenize(mut self) -> Vec<(Tok, Range)> {
        't: while self.cur < self.src.len() {
            let l = self.cur;
            if self.expect("//") {
                self.take(|c| c != b'\n');
                continue;
            }
            if let Some(_) = self.take(is_whitespace) {
                continue;
            }
            if let Some((word, range)) = self.take(is_ascii_digit) {
                self.toks.push((Tok::Int(word.parse().unwrap_or(0)), range));
                continue;
            }
            if let Some((word, range)) = self.take(is_id_char) {
                self.toks.push((Tok::Id(word.into()), range));
                continue;
            }
            if self.c() == b'"' {
                self.cur += 1;
                let p = |c: u8| c != b'"' && c != b'\n' && c != 0;
                while p(self.c()) {
                    self.cur += 1;
                }
                let r = self.cur;
                self.cur += 1;
                let word = self.src[l + 1..r].into();
                self.toks.push((Tok::Str(word), (l, r + 1)));
                continue;
            }
            for pun in PUNS {
                if self.expect(pun) {
                    self.toks.push((Tok::Pun(pun), (l, self.cur)));
                    continue 't;
                }
            }
            self.cur += 1;
            self.toks.push((Tok::Err("?".into()), (l, self.cur)));
        }
        self.toks.push((Tok::Eof, (self.cur, self.cur)));
        self.toks
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
