use crate::{
    syntax::syntax_token::SyntaxToken,
    token::{
        token_data::TokenData,
        token_kind::TokenKind,
        tokenize_rules::{tokenize_advance, MyTokenizerHost},
    },
};
use lc_utils::{
    deque_chan::DequeReceiver, parser::Parser, token_kind_trait::TokenKindTrait,
    token_with_trivia::TokenWithTrivia, tokenizer::Tokenizer,
};

pub(crate) trait LambdaParserHost {
    type TokenData;

    type AfterExpr;
    type AfterDecl;
    type AfterRoot;

    fn after_number_expr(&mut self, token: Self::TokenData) -> Self::AfterExpr;
    fn after_ident_expr(&mut self, token: Self::TokenData) -> Self::AfterExpr;

    fn after_let_decl(
        &mut self,
        keyword: Self::TokenData,
        name_opt: Option<Self::TokenData>,
        equal_opt: Option<Self::TokenData>,
        init_opt: Option<Self::AfterExpr>,
        semi_opt: Option<Self::TokenData>,
    ) -> Self::AfterDecl;

    fn after_root(&mut self, eof: Self::TokenData) -> Self::AfterRoot;
}

pub(crate) struct LambdaParser<'a, 'h, H: LambdaParserHost> {
    source_code: &'a str,
    tokenizer: Tokenizer<'a, 'h, MyTokenizerHost<'h>>,
    rx: DequeReceiver<'h, TokenData>,
    lookahead: (TokenKind, usize),
    next: Option<SyntaxToken<'a>>,
    count: usize,
    last_index: usize,
    pub(crate) host: &'h mut H,
}

impl<'a, 'h, H: LambdaParserHost> LambdaParser<'a, 'h, H> {
    pub(crate) fn new(
        source_code: &'a str,
        tokenizer: Tokenizer<'a, 'h, MyTokenizerHost<'h>>,
        rx: DequeReceiver<'h, TokenData>,
        host: &'h mut H,
    ) -> Self {
        let mut parser = Self {
            source_code,
            tokenizer,
            rx,
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
}

impl<'a, 'h, H: LambdaParserHost> Parser for LambdaParser<'a, 'h, H> {
    type TokenKind = TokenKind;
    type TokenData = SyntaxToken<'a>;
    type Host = H;

    fn host_mut(&mut self) -> &mut Self::Host {
        self.host
    }

    fn nth(&self, index: usize) -> TokenKind {
        // LL(1) だから
        assert_eq!(index, 0);

        match &self.next {
            Some(token_data) => token_data.kind,
            None => TokenKind::Eof,
        }
    }

    fn nth_data(&self, index: usize) -> Option<&SyntaxToken<'a>> {
        // LL(1) だから
        assert_eq!(index, 0);

        self.next.as_ref()
    }

    fn bump(&mut self) -> Self::TokenData {
        let next = self.next.take().unwrap();
        self.do_advance();
        next
    }

    fn skip(&mut self) {
        todo!()
    }
}

struct MyTokenStream<'a, 'h, 'p, H: LambdaParserHost> {
    parser: &'p mut LambdaParser<'a, 'h, H>,
}

impl<'a, 'h, 'p, H: LambdaParserHost> MyTokenStream<'a, 'h, 'p, H> {
    fn new(parser: &'p mut LambdaParser<'a, 'h, H>) -> Self {
        Self { parser }
    }

    fn try_pop_front(&mut self) -> Option<(TokenKind, usize)> {
        match self.parser.rx.pop_front() {
            Some(token_data) => {
                println!("rx -> {:?}", token_data);
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

fn do_lookahead<H: LambdaParserHost>(
    lookahead: &mut (TokenKind, usize),
    host: &mut MyTokenStream<H>,
) -> Option<TokenWithTrivia<TokenKind>> {
    if lookahead.0.is_eof() {
        return None;
    }

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
