use super::parser_host::LambdaParserHost;
use crate::{
    syntax::syntax_token::SyntaxToken,
    token::{
        token_kind::TokenKind,
        tokenize_rules::{tokenize_advance, MyTokenizerHost},
    },
};
use lc_utils::{token_stream::TokenStream, token_stream::TokenStreamHost, tokenizer::Tokenizer};
use std::collections::VecDeque;

pub(crate) struct MyTokenStreamState<'a> {
    tokenizer: Tokenizer<'a, MyTokenizerHost>,
}

impl<'a> MyTokenStreamState<'a> {
    fn try_pop_next_token(&mut self) -> Option<(TokenKind, usize)> {
        self.tokenizer
            .host
            .tokens
            .pop_front()
            .map(|token_data| (token_data.kind, token_data.len))
    }
}

impl<'a> TokenStreamHost for MyTokenStreamState<'a> {
    type TokenKind = TokenKind;

    fn advance(&mut self) -> (Self::TokenKind, usize) {
        let token_opt = self.try_pop_next_token();
        if let Some(token) = token_opt {
            return token;
        }

        if !tokenize_advance(&mut self.tokenizer) {
            return (TokenKind::Eof, 0);
        }

        self.try_pop_next_token().unwrap_or((TokenKind::Eof, 0))
    }
}

pub(crate) struct LambdaParser<'a, H: LambdaParserHost<'a>> {
    source_code: &'a str,
    token_stream: TokenStream<MyTokenStreamState<'a>>,
    next: Option<SyntaxToken<'a>>,
    count: usize,
    last_index: usize,
    pub(crate) host: H,
}

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    pub(crate) fn new(source_code: &'a str, host: H) -> Self {
        let tokenizer_host = MyTokenizerHost::new(VecDeque::new());
        let tokenizer = Tokenizer::new(source_code, tokenizer_host);

        let mut parser = Self {
            source_code,
            token_stream: TokenStream::new(MyTokenStreamState { tokenizer }),
            count: 0,
            last_index: 0,
            next: None,
            host,
        };
        parser.do_advance();
        parser
    }

    fn do_advance(&mut self) {
        debug_assert!(self.next.is_none());
        let token = self.token_stream.advance();

        let id = self.count;
        let start = self.last_index + token.leading_len;
        let end = start + token.body_len;
        let next = SyntaxToken {
            index: id,
            kind: token.kind,
            text: &self.source_code[start..end],
        };

        self.count += 1;
        self.last_index += token.total_len();
        self.next = Some(next);
    }

    pub(crate) fn nth(&self, index: usize) -> TokenKind {
        // LL(1) だから
        assert_eq!(index, 0);

        match &self.next {
            Some(token_data) => token_data.kind,
            None => TokenKind::Eof,
        }
    }

    pub(crate) fn nth_data(&self, index: usize) -> Option<&SyntaxToken<'a>> {
        // LL(1) だから
        assert_eq!(index, 0);

        self.next.as_ref()
    }

    pub(crate) fn next(&self) -> TokenKind {
        self.nth(0)
    }

    pub(crate) fn next_data(&self) -> Option<&SyntaxToken<'a>> {
        self.nth_data(0)
    }

    pub(crate) fn bump(&mut self) -> SyntaxToken<'a> {
        let next = self.next.take().unwrap();
        self.do_advance();
        next
    }

    pub(crate) fn eat(&mut self, kind: TokenKind) -> Option<SyntaxToken<'a>> {
        if self.next() == kind {
            Some(self.bump())
        } else {
            None
        }
    }

    pub(crate) fn skip(&mut self) {
        todo!("{:?}", self.next_data())
    }
}
