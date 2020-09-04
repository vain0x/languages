pub trait TokenKindTrait: Copy + Eq {
    fn is_eof(self) -> bool;
    fn is_leading_trivia(self) -> bool;
    fn is_trailing_trivia(self) -> bool;
}
