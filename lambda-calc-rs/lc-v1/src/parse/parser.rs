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

pub(crate) trait LambdaParserHost<'a> {
    type BeforeParamList;
    type AfterParamList;
    type BeforeArgList;
    type AfterArgList;
    type AfterExpr;
    type AfterDecl;
    type AfterRoot;

    fn before_param_list(&mut self, open_paren: SyntaxToken<'a>) -> Self::BeforeParamList;
    fn after_param(
        &mut self,
        name: SyntaxToken<'a>,
        comma_pot: Option<SyntaxToken<'a>>,
        param_list: &mut Self::BeforeParamList,
    );
    fn after_param_list(
        &mut self,
        close_paren_opt: Option<SyntaxToken<'a>>,
        param_list: Self::BeforeParamList,
    ) -> Self::AfterParamList;

    fn before_arg_list(&mut self, open_paren: SyntaxToken<'a>) -> Self::BeforeArgList;
    fn after_arg(
        &mut self,
        expr: Self::AfterExpr,
        comma_pot: Option<SyntaxToken<'a>>,
        arg_list: &mut Self::BeforeArgList,
    );
    fn after_arg_list(
        &mut self,
        close_paren_opt: Option<SyntaxToken<'a>>,
        arg_list: Self::BeforeArgList,
    ) -> Self::AfterArgList;

    fn after_true_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_false_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_number_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_ident_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_call_expr(
        &mut self,
        callee: Self::AfterExpr,
        arg_list: Self::AfterArgList,
    ) -> Self::AfterExpr;
    fn after_fn_expr(
        &mut self,
        keyword: SyntaxToken<'a>,
        param_list_opt: Option<Self::AfterParamList>,
        body_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr;

    fn after_let_decl(
        &mut self,
        keyword: SyntaxToken<'a>,
        name_opt: Option<SyntaxToken<'a>>,
        equal_opt: Option<SyntaxToken<'a>>,
        init_opt: Option<Self::AfterExpr>,
        semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl;

    fn after_root(&mut self, decls: Vec<Self::AfterDecl>, eof: SyntaxToken<'a>) -> Self::AfterRoot;
}

pub(crate) struct LambdaParser<'a, H: LambdaParserHost<'a>> {
    source_code: &'a str,
    tokenizer: Tokenizer<'a, 'a, MyTokenizerHost<'a>>,
    rx: DequeReceiver<'a, TokenData>,
    lookahead: (TokenKind, usize),
    next: Option<SyntaxToken<'a>>,
    count: usize,
    last_index: usize,
    pub(crate) host: &'a mut H,
}

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    pub(crate) fn new(
        source_code: &'a str,
        tokenizer: Tokenizer<'a, 'a, MyTokenizerHost<'a>>,
        rx: DequeReceiver<'a, TokenData>,
        host: &'a mut H,
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

impl<'a, H: LambdaParserHost<'a>> Parser for LambdaParser<'a, H> {
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

    fn bump(&mut self) -> SyntaxToken<'a> {
        let next = self.next.take().unwrap();
        self.do_advance();
        next
    }

    fn skip(&mut self) {
        todo!()
    }
}

struct MyTokenStream<'a, 'p, H: LambdaParserHost<'a>> {
    parser: &'p mut LambdaParser<'a, H>,
}

impl<'a, 'p, H: LambdaParserHost<'a>> MyTokenStream<'a, 'p, H> {
    fn new(parser: &'p mut LambdaParser<'a, H>) -> Self {
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

fn do_lookahead<'a, 'p, H: LambdaParserHost<'a>>(
    lookahead: &mut (TokenKind, usize),
    host: &mut MyTokenStream<'a, 'p, H>,
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
