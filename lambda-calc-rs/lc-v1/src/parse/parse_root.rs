use super::{
    parse_decls::parse_decl,
    parser::{LambdaParser, LambdaParserHost},
};
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

pub(crate) fn parse_root<'a, H: LambdaParserHost<'a>>(
    px: &mut LambdaParser<'a, H>,
) -> H::AfterRoot {
    let mut decls = vec![];
    loop {
        if let TokenKind::Eof = px.next() {
            break;
        }

        let decl = match parse_decl(px) {
            Some(it) => it,
            None => {
                eprintln!("expected decl");
                px.skip();
                continue;
            }
        };

        decls.push(decl);
    }

    let eof = px.bump();
    px.host.after_root(decls, eof)
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
        let source_code = "let id = id; let x = 1;";

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
            ARoot {
                decls: [
                    Let(
                        ALetDecl {
                            name_opt: Some(
                                "id",
                            ),
                            init_opt: Some(
                                Var(
                                    "id",
                                ),
                            ),
                        },
                    ),
                    Let(
                        ALetDecl {
                            name_opt: Some(
                                "x",
                            ),
                            init_opt: Some(
                                Number(
                                    "1",
                                ),
                            ),
                        },
                    ),
                ],
                eof: Eof,
            }"#]];
        expect.assert_eq(&actual);
    }
}
