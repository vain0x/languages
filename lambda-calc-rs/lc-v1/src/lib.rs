mod ast {
    pub(crate) mod a_parser;
    pub(crate) mod a_tree;
}

mod eval {
    pub(crate) mod eval;
}

mod parse {
    pub(crate) mod parse_decls;
    pub(crate) mod parse_exprs;
    pub(crate) mod parse_root;
    pub(crate) mod parser;
}

// mod scope {
//     pub(crate) mod name_resolver;
//     pub(crate) mod scope_chain;
// }

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
        ast::a_parser::AstLambdaParserHost, ast::a_tree::Ast, context::Context,
        parse::parser::LambdaParser, token::tokenize_rules::MyTokenizerHost,
    };
    use lc_utils::{deque_chan::deque_chan, tokenizer::Tokenizer};
    use std::collections::VecDeque;

    pub fn evaluate(source_code: &str) -> String {
        let context = Context::new();
        let mut tokens = VecDeque::new();

        let (tx, rx) = deque_chan(&mut tokens);
        let mut tokenizer_host = MyTokenizerHost::new(tx);
        let tokenizer = Tokenizer::new(source_code, &mut tokenizer_host);

        let mut parser_host = AstLambdaParserHost {
            context: &context,
            // ...
        };
        let mut parser = LambdaParser::new(source_code, tokenizer, rx, &mut parser_host);
        let root = parser.parse_root();
        let ast = context.bump.alloc(Ast { root });

        crate::eval::eval::evaluate(ast)
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

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

    fn do_test_evaluate(input: &str, expect: Expect) {
        let actual = crate::rust_api::evaluate(input);
        expect.assert_eq(&actual)
    }

    #[test]
    fn test_evaluate() {
        do_test_evaluate(
            r#"
                let num = 42;
                let t = true;
                let f = false;

                // var expr
                let var = num;

                // fn expr
                let zero_fn = fn() 0;

                // call expr
                let _ = zero_fn();

                // if expr
                let _ = if true { 1 } else { 0 };
                let _ = if false { 1 } else { 0 };

                let id = fn(x) x;
                let _ = id(id)(id)(id)(zero_fn)();
            "#,
            expect![[r#"
                val num : number = 42;
                val t : bool = true;
                val f : bool = false;
                val var : number = 42;
                val zero_fn : fn(...) -> ... = <function>;
                val _ : number = 0;
                val _ : number = 1;
                val _ : number = 0;
                val id : fn(...) -> ... = <function>;
                val _ : number = 0;
            "#]],
        );
    }
}
