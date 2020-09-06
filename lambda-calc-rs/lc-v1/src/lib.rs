mod ast {
    pub(crate) mod a_parser;
    pub(crate) mod a_tree;
}

mod eval {
    pub(crate) mod code_gen;
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

        let mut parser_host = AstLambdaParserHost::new(&context);
        let mut parser = LambdaParser::new(source_code, tokenizer, rx, &mut parser_host);
        let root = parser.parse_root();
        let ast = context.bump.alloc(Ast { root });

        crate::eval::eval::evaluate(ast)
    }

    pub fn compile(source_code: &str) -> String {
        let context = Context::new();
        let mut tokens = VecDeque::new();

        let (tx, rx) = deque_chan(&mut tokens);
        let mut tokenizer_host = MyTokenizerHost::new(tx);
        let tokenizer = Tokenizer::new(source_code, &mut tokenizer_host);

        let mut parser_host = AstLambdaParserHost::new(&context);
        let mut parser = LambdaParser::new(source_code, tokenizer, rx, &mut parser_host);
        let root = parser.parse_root();
        let ast = context.bump.alloc(Ast { root });

        let program = crate::eval::code_gen::code_gen(ast);
        format!("{:#?}", program)
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

                // block expr
                {
                    let inner_var = 4;
                    inner_var
                }
                inner_var

                // if expr
                let _ = if true { 1 } else { 0 };
                let _ = if false { 1 } else { 0 };

                // expr decl
                42;
                let the_value = it;

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
                val it : number = 4;
                val it = <err>;
                ERROR: "unknown var inner_var"
                val _ : number = 1;
                val _ : number = 0;
                val it : number = 42;
                val the_value : number = 42;
                val id : fn(...) -> ... = <function>;
                val _ : number = 0;
            "#]],
        );
    }

    fn do_test_compile(input: &str, expect: Expect) {
        let actual = crate::rust_api::compile(input);
        expect.assert_eq(&actual)
    }

    #[test]
    fn test_compile() {
        do_test_compile(
            r#"
                0;
                let a = 42;

                {
                    let b = 1;
                    b;
                }
            "#,
            expect![[r#"
                Ok(
                    Program {
                        reg_count: 5,
                        labels: [],
                        fns: [],
                        codes: [
                            MovImm(
                                Reg(
                                    1,
                                ),
                                Int(
                                    0,
                                ),
                            ),
                            PrintVal(
                                "it",
                                Reg(
                                    1,
                                ),
                            ),
                            MovImm(
                                Reg(
                                    2,
                                ),
                                Int(
                                    42,
                                ),
                            ),
                            PrintVal(
                                "a",
                                Reg(
                                    2,
                                ),
                            ),
                            MovImm(
                                Reg(
                                    3,
                                ),
                                Int(
                                    1,
                                ),
                            ),
                            PrintVal(
                                "b",
                                Reg(
                                    3,
                                ),
                            ),
                            LoadStaticVar(
                                Reg(
                                    4,
                                ),
                                1,
                            ),
                            PrintVal(
                                "it",
                                Reg(
                                    4,
                                ),
                            ),
                            Exit,
                        ],
                    },
                )"#]],
        )
    }

    #[test]
    fn test_compile_call_prim() {
        do_test_compile(
            r#"
                int_add(2, 3)
            "#,
            expect![[r#"
                Ok(
                    Program {
                        reg_count: 5,
                        labels: [],
                        fns: [],
                        codes: [
                            MovImm(
                                Reg(
                                    1,
                                ),
                                Prim(
                                    IntAdd,
                                ),
                            ),
                            MovImm(
                                Reg(
                                    2,
                                ),
                                Int(
                                    2,
                                ),
                            ),
                            MovImm(
                                Reg(
                                    3,
                                ),
                                Int(
                                    3,
                                ),
                            ),
                            BeginCall(
                                Reg(
                                    1,
                                ),
                                2,
                            ),
                            StoreLocalVar(
                                0,
                                Reg(
                                    2,
                                ),
                            ),
                            StoreLocalVar(
                                1,
                                Reg(
                                    3,
                                ),
                            ),
                            EndCall(
                                Reg(
                                    1,
                                ),
                            ),
                            Mov(
                                Reg(
                                    4,
                                ),
                                Reg(
                                    0,
                                ),
                            ),
                            PrintVal(
                                "it",
                                Reg(
                                    4,
                                ),
                            ),
                            Exit,
                        ],
                    },
                )"#]],
        );
    }

    #[test]
    fn test_compile_if() {
        do_test_compile(
            r#"
                if false { 1 } else if false { 2 } else { 3 }
            "#,
            expect![[r#"
                Ok(
                    Program {
                        reg_count: 6,
                        labels: [
                            3,
                            6,
                            18,
                            9,
                            12,
                            15,
                        ],
                        fns: [],
                        codes: [
                            MovImm(
                                Reg(
                                    1,
                                ),
                                Bool(
                                    false,
                                ),
                            ),
                            JumpUnless(
                                2,
                                Reg(
                                    1,
                                ),
                            ),
                            LabelDecl(
                                0,
                            ),
                            MovImm(
                                Reg(
                                    2,
                                ),
                                Int(
                                    1,
                                ),
                            ),
                            Jump(
                                2,
                            ),
                            LabelDecl(
                                1,
                            ),
                            MovImm(
                                Reg(
                                    3,
                                ),
                                Bool(
                                    false,
                                ),
                            ),
                            JumpUnless(
                                5,
                                Reg(
                                    3,
                                ),
                            ),
                            LabelDecl(
                                3,
                            ),
                            MovImm(
                                Reg(
                                    4,
                                ),
                                Int(
                                    2,
                                ),
                            ),
                            Jump(
                                5,
                            ),
                            LabelDecl(
                                4,
                            ),
                            MovImm(
                                Reg(
                                    4,
                                ),
                                Int(
                                    3,
                                ),
                            ),
                            Jump(
                                5,
                            ),
                            LabelDecl(
                                5,
                            ),
                            Mov(
                                Reg(
                                    2,
                                ),
                                Reg(
                                    4,
                                ),
                            ),
                            Jump(
                                2,
                            ),
                            LabelDecl(
                                2,
                            ),
                            Mov(
                                Reg(
                                    5,
                                ),
                                Reg(
                                    2,
                                ),
                            ),
                            PrintVal(
                                "it",
                                Reg(
                                    5,
                                ),
                            ),
                            Exit,
                        ],
                    },
                )"#]],
        );
    }

    #[test]
    fn test_compile_fn_with_arity_zero() {
        do_test_compile(
            r#"
                let zero_fn = fn() 0;
                zero_fn();
            "#,
            expect![[r#"
                Ok(
                    Program {
                        reg_count: 4,
                        labels: [],
                        fns: [
                            FnInfo {
                                local_var_count: 0,
                                pc: 8,
                            },
                        ],
                        codes: [
                            MovImm(
                                Reg(
                                    1,
                                ),
                                Fn(
                                    0,
                                ),
                            ),
                            PrintVal(
                                "zero_fn",
                                Reg(
                                    1,
                                ),
                            ),
                            LoadStaticVar(
                                Reg(
                                    2,
                                ),
                                0,
                            ),
                            BeginCall(
                                Reg(
                                    2,
                                ),
                                0,
                            ),
                            EndCall(
                                Reg(
                                    2,
                                ),
                            ),
                            Mov(
                                Reg(
                                    3,
                                ),
                                Reg(
                                    0,
                                ),
                            ),
                            PrintVal(
                                "it",
                                Reg(
                                    3,
                                ),
                            ),
                            Exit,
                            MovImm(
                                Reg(
                                    0,
                                ),
                                Int(
                                    0,
                                ),
                            ),
                            Return,
                        ],
                    },
                )"#]],
        );
    }

    #[test]
    fn test_compile_fn_with_params() {
        do_test_compile(
            r#"
                let twice = fn(f, x) f(f(x));
                twice(fn(x) x, 0);
            "#,
            expect![[r#"
                Ok(
                    Program {
                        reg_count: 10,
                        labels: [],
                        fns: [
                            FnInfo {
                                local_var_count: 2,
                                pc: 12,
                            },
                            FnInfo {
                                local_var_count: 1,
                                pc: 24,
                            },
                        ],
                        codes: [
                            MovImm(
                                Reg(
                                    5,
                                ),
                                Fn(
                                    0,
                                ),
                            ),
                            PrintVal(
                                "twice",
                                Reg(
                                    5,
                                ),
                            ),
                            LoadStaticVar(
                                Reg(
                                    6,
                                ),
                                0,
                            ),
                            MovImm(
                                Reg(
                                    7,
                                ),
                                Fn(
                                    1,
                                ),
                            ),
                            MovImm(
                                Reg(
                                    8,
                                ),
                                Int(
                                    0,
                                ),
                            ),
                            BeginCall(
                                Reg(
                                    6,
                                ),
                                2,
                            ),
                            StoreLocalVar(
                                0,
                                Reg(
                                    7,
                                ),
                            ),
                            StoreLocalVar(
                                1,
                                Reg(
                                    8,
                                ),
                            ),
                            EndCall(
                                Reg(
                                    6,
                                ),
                            ),
                            Mov(
                                Reg(
                                    9,
                                ),
                                Reg(
                                    0,
                                ),
                            ),
                            PrintVal(
                                "it",
                                Reg(
                                    9,
                                ),
                            ),
                            Exit,
                            LoadLocalVar(
                                Reg(
                                    1,
                                ),
                                0,
                            ),
                            LoadLocalVar(
                                Reg(
                                    2,
                                ),
                                0,
                            ),
                            LoadLocalVar(
                                Reg(
                                    3,
                                ),
                                1,
                            ),
                            BeginCall(
                                Reg(
                                    2,
                                ),
                                1,
                            ),
                            StoreLocalVar(
                                0,
                                Reg(
                                    3,
                                ),
                            ),
                            EndCall(
                                Reg(
                                    2,
                                ),
                            ),
                            Mov(
                                Reg(
                                    4,
                                ),
                                Reg(
                                    0,
                                ),
                            ),
                            BeginCall(
                                Reg(
                                    1,
                                ),
                                1,
                            ),
                            StoreLocalVar(
                                0,
                                Reg(
                                    4,
                                ),
                            ),
                            EndCall(
                                Reg(
                                    1,
                                ),
                            ),
                            Mov(
                                Reg(
                                    0,
                                ),
                                Reg(
                                    0,
                                ),
                            ),
                            Return,
                            LoadLocalVar(
                                Reg(
                                    0,
                                ),
                                0,
                            ),
                            Return,
                        ],
                    },
                )"#]],
        );
    }

    #[test]
    fn test_compile_scope_error() {
        do_test_compile(
            r#"
                {
                    let inner = 1;
                }
                inner;
            "#,
            expect![[r#"
                Err(
                    "undefined value \"inner\"",
                )"#]],
        )
    }
}
