pub struct TokenWithTrivia<K> {
    pub kind: K,
    pub leading_len: usize,
    pub body_len: usize,
    pub trailing_len: usize,
}

impl<K> TokenWithTrivia<K> {
    pub fn total_len(&self) -> usize {
        self.leading_len + self.body_len + self.trailing_len
    }
}
