use super::*;
use std::cell::RefCell;
use std::rc::Rc;

struct Tokenizer<'a> {
    syntax: &'a mut Syntax,
    module_id: ModuleId,
    doc: Rc<Doc>,
    current: usize,
    tick: RefCell<usize>,
}

impl Tokenizer<'_> {
    fn add_token(&mut self, kind: TokenKind, span: Span) {
        let module_id = self.module_id;
        let token_id = TokenId::new(self.syntax.tokens.len());
        self.syntax.tokens.insert(
            token_id,
            Token {
                kind,
                module_id,
                span,
            },
        );
    }

    fn next_char(&self) -> u8 {
        debug_assert!({
            *self.tick.borrow_mut() += 1;
            *self.tick.borrow() < 1_000_000
        });

        if self.at_eof() {
            return 0;
        }
        self.src().as_bytes()[self.current]
    }

    fn at_eof(&self) -> bool {
        self.current >= self.src().len()
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
        Some((self.src()[l..r].to_string(), (l, r)))
    }

    fn reads(&mut self, prefix: &str) -> bool {
        if self.src()[self.current..].starts_with(prefix) {
            self.current += prefix.len();
            return true;
        }
        false
    }

    fn look_esc(&self) -> (u8, usize) {
        assert_eq!(self.next_char(), b'\\');

        match self.src().as_bytes()[self.current + 1] {
            b'\0' => (b'\0', 1),
            b'0' => (b'\0', 2),
            b'r' => (b'\r', 2),
            b'n' => (b'\n', 2),
            b't' => (b'\t', 2),
            c => (c, 2),
        }
    }

    fn read_char(&mut self) {
        assert_eq!(self.next_char(), b'\'');

        let l = self.current;
        self.current += 1;

        let (c, n) = match self.next_char() {
            b'\0' | b'\r' | b'\n' => (b'\0', 0),
            b'\\' => self.look_esc(),
            c => (c, 1),
        };
        self.current += n;

        if !self.reads("'") {
            self.add_token(TokenKind::Err, (l, self.current));
            return;
        }
        self.add_token(TokenKind::Char(c), (l, self.current))
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
            if self.next_char() == b'\'' {
                self.read_char();
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

            let char_len = next_char_len(&self.src()[self.current..]);
            self.current += char_len;
            self.add_token(TokenKind::Err, (l, self.current));
        }
        self.add_token(TokenKind::Eof, (self.current, self.current));
    }
}

impl BorrowDoc for Tokenizer<'_> {
    fn doc(&self) -> &Doc {
        &self.doc
    }
}

fn is_ascii_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_ident_char(c: u8) -> bool {
    (b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z' || is_ascii_digit(c) || c == b'_')
}

fn is_op_char(c: u8) -> bool {
    b"!*+-./%<=>?@^~&|".contains(&c)
}

fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

fn next_char_len(src: &str) -> usize {
    src.chars()
        .next()
        .expect("Any character is expected")
        .len_utf8()
}

pub(crate) fn tokenize(syntax: &'_ mut Syntax, module_id: ModuleId, doc: Rc<Doc>) -> TokenId {
    let root_token_id = TokenId::new(syntax.tokens.len());
    let mut tokenizer = Tokenizer {
        syntax,
        module_id,
        doc,
        current: 0,
        tick: RefCell::new(0),
    };
    tokenizer.tokenize();
    root_token_id
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_char_len() {
        assert_eq!(next_char_len("😀😀"), "😀".len());
    }

    #[test]
    fn test_tokenizer_does_not_hang() {
        let mut syntax = Syntax::default();
        let doc = Rc::new(Doc::new(
            format!("{}:{}", file!(), line!()),
            Rc::new("#!/bin/bash\necho こんにちは世界😀".to_string()),
        ));
        tokenize(&mut syntax, ModuleId::new(0), doc);
        assert!(syntax.tokens.len() != 0);
    }
}
