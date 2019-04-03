use std::fmt;

#[derive(Clone, Debug)]
pub(crate) enum SyntaxError {
    InvalidChar,
    UnexpectedEof,
    ExpectedChar(char),
    ExpectedEof,
    ExpectedEither(Vec<String>),
    ExpectedTy,
    ExpectedExp,
    MissingSingleQuote,
    UnknownEscapeSequence,
    NonSingleCharLiteral,
    MissingDoubleQuote,
    Unimplemented(&'static str),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::InvalidChar => write!(f, "Invalid character"),
            SyntaxError::UnexpectedEof => write!(f, "Unexpected EOF"),
            SyntaxError::ExpectedChar(c) => write!(f, "Expected '{}'", c),
            SyntaxError::ExpectedEof => write!(f, "Expected EOF"),
            SyntaxError::ExpectedEither(candidates) => {
                write!(f, "Expected '{}'", candidates.join("', '"))
            }
            SyntaxError::ExpectedTy => write!(f, "Expected a type"),
            SyntaxError::ExpectedExp => write!(f, "Expected an expression"),
            SyntaxError::MissingSingleQuote => write!(f, "Single quote missing"),
            SyntaxError::UnknownEscapeSequence => write!(f, "Unknown escape sequence"),
            SyntaxError::NonSingleCharLiteral => write!(f, "Expected exactly one ASCII character"),
            SyntaxError::MissingDoubleQuote => write!(f, "Double quote missing"),
            SyntaxError::Unimplemented(message) => write!(f, "{}", message),
        }
    }
}
