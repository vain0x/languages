pub trait Parser {
    type TokenKind: Copy + Eq;
    type TokenData;
    type Host;

    fn host_mut(&mut self) -> &mut Self::Host;

    fn nth(&self, index: usize) -> Self::TokenKind;

    fn nth_data(&self, index: usize) -> Option<&Self::TokenData>;

    fn next(&self) -> Self::TokenKind {
        self.nth(0)
    }

    fn next_data(&self) -> Option<&Self::TokenData> {
        self.nth_data(0)
    }

    fn at_eof(&self) -> bool {
        self.next_data().is_none()
    }

    fn bump(&mut self) -> Self::TokenData;

    fn eat(&mut self, kind: Self::TokenKind) -> Option<Self::TokenData> {
        if self.next() == kind {
            Some(self.bump())
        } else {
            None
        }
    }

    fn skip(&mut self);
}
