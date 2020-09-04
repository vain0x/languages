use super::parser::{LambdaParser, LambdaParserHost};
use crate::token::{
    token_data::TokenData,
    token_kind::TokenKind,
    tokenize_rules::{tokenize_advance, MyTokenizerHost},
};
use crate::{syntax::syntax_token::SyntaxToken, utils::*};
use lc_utils::{
    parser::Parser,
    token_stream::{TokenStream, TokenStreamHost},
    tokenizer::Tokenizer,
};

pub(crate) fn parse_root<'a, H: LambdaParserHost>(
    px: &mut LambdaParser<'a, '_, H>,
) -> Vec<SyntaxToken<'a>> {
    let mut tokens = vec![];
    loop {
        match px.next() {
            TokenKind::Eof => break,
            _ => tokens.push(px.bump()),
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::a_parser::AstLambdaParserHost, context::Context, parse::parser::LambdaParser,
    };
    use expect_test::expect;
    use lc_utils::deque_chan::deque_chan;
    use std::collections::VecDeque;

    #[test]
    fn test_parse() {
        let source_code = "let id = id;";

        let mut context = Context::new();
        let mut tokens = VecDeque::new();

        let (tx, rx) = deque_chan(&mut tokens);
        let mut tokenizer_host = MyTokenizerHost::new(tx);
        let tokenizer = Tokenizer::new(source_code, &mut tokenizer_host);

        let mut parser_host = AstLambdaParserHost {
            context: &mut context,
            // ...
        };
        let mut parser = LambdaParser::new(source_code, tokenizer, rx, &mut parser_host);
        let tokens = parse_root(&mut parser);

        let actual = format!("{:#?}", tokens);

        let expect = expect![[r#"
            [
                Let,
                "id",
                Equal,
                "id",
                SemiColon,
            ]"#]];
        expect.assert_eq(&actual);
    }
}
