mod ast {
    pub(crate) mod a_parser;
    pub(crate) mod a_tree;
}

mod parse {
    pub(crate) mod parse_decls;
    pub(crate) mod parse_exprs;
    pub(crate) mod parse_root;
    pub(crate) mod parser;
}

mod syntax {
    pub(crate) mod syntax_token;
    pub(crate) mod syntax_tree;
}

mod token {
    pub(crate) mod token_data;
    pub(crate) mod token_kind;
    pub(crate) mod tokenize_rules;
}

mod utils {
    pub(crate) use bumpalo::{boxed::Box as BumpaloBox, collections::Vec as BumpaloVec};
}

mod context {
    use crate::utils::*;

    // #[derive(Copy, Clone)]
    // pub(crate) struct ContextRef<'a> {
    //     context: &'a Context,
    // }

    pub(crate) struct Context {
        pub(crate) bump: bumpalo::Bump,
    }

    impl Context {
        pub(crate) fn new() -> Self {
            Self {
                bump: bumpalo::Bump::new(),
            }
        }

        pub(crate) fn allocate_iter<T>(
            &self,
            iter: impl IntoIterator<Item = T>,
        ) -> BumpaloVec<'_, T> {
            BumpaloVec::from_iter_in(iter, &self.bump)
        }
    }
}

pub mod rust_api {
    use crate::{
        ast::a_parser::AstLambdaParserHost, context::Context, parse::parser::LambdaParser,
        token::tokenize_rules::MyTokenizerHost,
    };
    use lc_utils::{deque_chan::deque_chan, tokenizer::Tokenizer};
    use std::collections::VecDeque;

    pub fn syntax_tree(source_code: &str) -> String {
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
        let tokens = parser.parse_root();
        format!("{:#?}", tokens)
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn snapshot_test() {
        let actual = "OK!";
        let expect = expect![[r#"OK!"#]];
        expect.assert_eq(actual);
    }
}
