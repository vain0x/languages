pub trait TokenizerHost {
    type Kind: Copy;

    fn on_token(&mut self, text: &str, kind: Self::Kind, start: usize, end: usize);
}

pub struct Tokenizer<'a, H> {
    source_code: &'a str,
    last_index: usize,
    current_index: usize,
    pub host: H,
}

impl<'a, H: TokenizerHost> Tokenizer<'a, H> {
    pub fn new(source_code: &'a str, host: H) -> Self {
        Tokenizer {
            source_code,
            last_index: 0,
            current_index: 0,
            host,
        }
    }

    pub fn commit(&mut self, kind: H::Kind) {
        let text = self.current_text();
        self.host
            .on_token(text, kind, self.last_index, self.current_index);
        self.last_index = self.current_index;

        self.assert_invariants();
    }

    pub fn finish(self) {
        assert_eq!(self.current_index, self.last_index);
        assert_eq!(self.current_index, self.source_code.len());
    }
}

impl<'a, H> Tokenizer<'a, H> {
    fn assert_invariants(&self) {
        assert!(self.last_index <= self.current_index);
        assert!(self.source_code.is_char_boundary(self.last_index));
        assert!(self.source_code.is_char_boundary(self.current_index));
    }

    pub fn following_text(&self) -> &'a str {
        &self.source_code[self.current_index..]
    }

    pub fn current_text(&self) -> &'a str {
        &self.source_code[self.last_index..self.current_index]
    }

    pub fn at_eof(&self) -> bool {
        self.current_index >= self.source_code.len()
    }

    pub fn nth(&self, index: usize) -> char {
        self.source_code[self.current_index..]
            .chars()
            .nth(index)
            .unwrap_or('\0')
    }

    pub fn next(&self) -> char {
        self.nth(0)
    }

    pub fn is_followed_by(&self, text: &str) -> bool {
        self.source_code[self.current_index..].starts_with(text)
    }

    pub fn bump_bytes(&mut self, len: usize) {
        assert!(self.source_code.is_char_boundary(self.current_index + len));

        self.current_index += len;

        self.assert_invariants();
    }

    pub fn bump_chars(&mut self, count: usize) {
        let len = self
            .following_text()
            .chars()
            .take(count)
            .fold(0, |len, c| len + c.len_utf8());
        self.bump_bytes(len);
    }

    pub fn bump(&mut self) {
        self.bump_bytes(self.next().len_utf8());
    }

    pub fn bump_line(&mut self) {
        let len = match self.following_text().find('\n') {
            None => self.following_text().len(),
            Some(offset) => {
                let at_crlf = {
                    let i = self.current_index + offset;
                    i >= 1
                        && self.source_code.is_char_boundary(i - 1)
                        && self.source_code.as_bytes()[i - 1] == b'\r'
                };
                if at_crlf {
                    offset - 1
                } else {
                    offset
                }
            }
        };

        self.bump_bytes(len);
    }

    pub fn eat(&mut self, text: &str) -> bool {
        if self.following_text().starts_with(text) {
            self.bump_bytes(text.len());
            true
        } else {
            false
        }
    }
}

pub struct TokenizationCommit<'a, K> {
    pub text: &'a str,
    pub kind: K,
    pub start: usize,
    pub end: usize,
}

impl<'a, K: Copy> TokenizationCommit<'a, K> {
    pub fn len(&self) -> usize {
        self.end - self.start
    }
}
