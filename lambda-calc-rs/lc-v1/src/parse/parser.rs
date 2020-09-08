use super::parser_host::LambdaParserHost;
use crate::{
    syntax::syntax_token::SyntaxToken,
    token::{
        token_kind::TokenKind,
        tokenize_rules::{tokenize_advance, MyTokenizerHost},
    },
};
use lc_utils::{
    token_kind_trait::TokenKindTrait, token_with_trivia::TokenWithTrivia, tokenizer::Tokenizer,
};
use std::collections::VecDeque;

pub(crate) struct LambdaParser<'a, 'h, H: LambdaParserHost<'a>> {
    source_code: &'a str,
    tokenizer: Tokenizer<'a, MyTokenizerHost>,
    lookahead: (TokenKind, usize),
    next: Option<SyntaxToken<'a>>,
    count: usize,
    last_index: usize,
    pub(crate) host: &'h mut H,
}

impl<'a, 'h, H: LambdaParserHost<'a>> LambdaParser<'a, 'h, H> {
    pub(crate) fn new(source_code: &'a str, host: &'h mut H) -> Self {
        let tokenizer_host = MyTokenizerHost::new(VecDeque::new());
        let tokenizer = Tokenizer::new(source_code, tokenizer_host);

        let mut parser = Self {
            source_code,
            tokenizer,
            count: 0,
            last_index: 0,
            lookahead: (TokenKind::Blank, 0),
            next: None,
            host,
        };
        parser.do_advance();
        parser
    }

    fn do_advance(&mut self) {
        let index = self.count;
        let start = self.last_index;
        let mut lookahead = self.lookahead;
        let token = {
            let opt = do_lookahead(&mut lookahead, &mut MyTokenStream::new(self));
            self.lookahead = lookahead;
            match opt {
                Some(it) => it,
                None => return,
            }
        };

        self.next = Some(SyntaxToken {
            index,
            kind: token.kind,
            text: &self.source_code
                [start + token.leading_len..start + token.leading_len + token.body_len],
        });
        self.last_index += token.total_len();
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

struct MyTokenStream<'a, 'h, 'p, H: LambdaParserHost<'a>> {
    parser: &'p mut LambdaParser<'a, 'h, H>,
}

impl<'a, 'h, 'p, H: LambdaParserHost<'a>> MyTokenStream<'a, 'h, 'p, H> {
    fn new(parser: &'p mut LambdaParser<'a, 'h, H>) -> Self {
        Self { parser }
    }

    fn try_pop_front(&mut self) -> Option<(TokenKind, usize)> {
        match self.parser.tokenizer.host.tokens.pop_front() {
            Some(token_data) => {
                // println!("rx -> {:?}", token_data);
                Some((token_data.kind, token_data.len))
            }
            None => None,
        }
    }

    pub(crate) fn next(&mut self) -> (TokenKind, usize) {
        let token_opt = self.try_pop_front();
        if let Some(token) = token_opt {
            self.parser.count += 1;
            return token;
        }

        if !tokenize_advance(&mut self.parser.tokenizer) {
            return (TokenKind::Eof, 0);
        }

        self.parser.count += 1;
        self.try_pop_front().unwrap_or((TokenKind::Eof, 0))
    }
}

fn do_lookahead<'a, 'p, H: LambdaParserHost<'a>>(
    lookahead: &mut (TokenKind, usize),
    host: &mut MyTokenStream<'a, '_, 'p, H>,
) -> Option<TokenWithTrivia<TokenKind>> {
    let mut leading_len = 0_usize;
    while lookahead.0.is_leading_trivia() {
        leading_len += lookahead.1;
        *lookahead = host.next();
    }

    let (kind, body_len) = *lookahead;
    *lookahead = host.next();

    let mut trailing_len = 0_usize;
    while lookahead.0.is_trailing_trivia() {
        trailing_len += lookahead.1;
        *lookahead = host.next();
    }

    Some(TokenWithTrivia {
        kind,
        leading_len,
        body_len,
        trailing_len,
    })
}
