mod ast {
    pub(crate) mod a_parser;
    pub(crate) mod a_tree;
}

mod eval {
    pub(crate) mod code_gen;
    pub(crate) mod eval;
    pub(crate) mod type_check;

    use std::fmt::{self, Debug, Formatter};
}

mod parse {
    pub(crate) mod parse_exprs;
    pub(crate) mod parse_root;
    pub(crate) mod parse_stmts;
    pub(crate) mod parse_tys;
    pub(crate) mod parser;
    pub(crate) mod parser_host;
}

mod semantics {
    pub(crate) mod analyze;
    pub(crate) mod local_symbol;
    pub(crate) mod prim;
    pub(crate) mod prim_ty;
    pub(crate) mod symbol;
    pub(crate) mod ty;
    pub(crate) mod scope {
        pub(crate) mod name_resolution;
        pub(crate) mod scope;
        pub(crate) mod scope_chain;
        pub(crate) mod scope_kind;
        pub(crate) mod scope_resolver;
    }

    use std::fmt::{self, Debug, Formatter};
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
        ast::a_parser::AstLambdaParserHost, ast::a_tree::Ast, context::Context,
        parse::parser::LambdaParser,
    };

    pub fn evaluate(source_code: &str) -> String {
        let context = Context::new();

        let parser_host = AstLambdaParserHost::new(&context);
        let mut parser = LambdaParser::new(source_code, parser_host);
        let root = parser.parse_root();
        let name_res = parser.host.take_output();
        // eprintln!("name_res = {:#?}", parser.host.name_res);
        let ast = context.bump.alloc(Ast {
            root,
            name_res: name_res.ident_symbols,
            fn_escapes: name_res.fn_escapes,
        });

        crate::eval::eval::evaluate(ast)
    }

    pub fn type_check(source_code: &str) -> String {
        let context = Context::new();

        let parser_host = AstLambdaParserHost::new(&context);
        let mut parser = LambdaParser::new(source_code, parser_host);
        let root = parser.parse_root();
        let name_res = parser.host.take_output();
        let ast = context.bump.alloc(Ast {
            root,
            name_res: name_res.ident_symbols,
            fn_escapes: name_res.fn_escapes,
        });

        let (_, errors) = crate::semantics::analyze::analyze(ast, &context.bump);
        format!("{:#?}", errors)
    }

    pub fn compile(source_code: &str) -> String {
        let context = Context::new();

        let parser_host = AstLambdaParserHost::new(&context);
        let mut parser = LambdaParser::new(source_code, parser_host);
        let root = parser.parse_root();
        let name_res = parser.host.take_output();
        let ast = context.bump.alloc(Ast {
            root,
            name_res: name_res.ident_symbols,
            fn_escapes: name_res.fn_escapes,
        });

        let (program, _) = crate::semantics::analyze::analyze(ast, &context.bump);
        // let program = crate::eval::code_gen::code_gen(ast);
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

                // expr stmt
                42;

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
                val id : fn(...) -> ... = <function>;
                val _ : number = 0;
            "#]],
        );
    }

    #[test]
    fn test_evaluate_prim() {
        do_test_evaluate(
            r#"
                int_add(2, 3);
                int_add();
                int_add(1, 2, 3);
                int_add(true, false);
            "#,
            expect![[r#"
                val it : number = 5;
                val it = <err>;
                ERROR: "arity error"
                val it = <err>;
                ERROR: "arity error"
                val it = <err>;
                ERROR: "type error"
            "#]],
        );
    }

    #[test]
    fn test_evaluate_closure() {
        do_test_evaluate(
            r#"
                let con = fn(x) {
                    fn() x
                };
                let f1 = con(1);
                f1();
                let f2 = con(2);
                f2();
            "#,
            expect![[r#"
                val con : fn(...) -> ... = <function>;
                val f1 : fn(...) -> ... = <function>;
                val it : number = 1;
                val f2 : fn(...) -> ... = <function>;
                val it : number = 2;
            "#]],
        );
    }

    fn do_test_type_check(input: &str, expect: Expect) {
        let actual = crate::rust_api::type_check(input);
        expect.assert_eq(&actual)
    }

    #[test]
    fn test_type_check() {
        do_test_type_check(
            r#"
                let a = 1;
                let f = fn(x: int) x;
                let b = int_eq(2, 3);
                let ff = fn(f: fn(bool, int) -> fn()) f;
            "#,
            expect![[r#"
                [
                    "static a : int",
                    "param x : int",
                    "static f : fn(int) -> int",
                    "static b : bool",
                    "param f : fn(bool, int) -> fn() -> unit",
                    "static ff : fn(fn(bool, int) -> fn() -> unit) -> fn(bool, int) -> fn() -> unit",
                ]"#]],
        );
    }

    #[test]
    fn test_escaping_check() {
        do_test_type_check(
            r#"
                let PI = 3;
                fn(x: int) {
                    let a = 4; // escaping
                    fn(y: int) {
                        int_add(x, PI); // PI is global
                        int_add(y, a) // y is local
                    }
                }
            "#,
            expect![[r#"
                [
                    "static PI : int",
                    "escaping: Param { id: 2 }",
                    "param x : int",
                    "val a : int",
                    "escaping: LocalVar { id: 3 }, Param { id: 2 }",
                    "param y : int",
                ]"#]],
        );
    }

    #[test]
    #[should_panic(expected = "type mismatch")]
    fn test_type_check_error() {
        do_test_type_check(
            r#"
                (fn(x: int) {})(true);
            "#,
            expect![[]],
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
                Program {
                    reg_count: 1,
                    blocks: [
                        BlockInfo {
                            codes: [
                                print("it", 0_int),
                                store s_1 <- 42_int,
                                print("a", 42_int),
                                store s_2 <- 1_int,
                                print("b", 1_int),
                                print("it", s_2),
                            ],
                            cont: exit,
                        },
                    ],
                    fns: [],
                    codes: [],
                }"#]],
        )
    }

    #[test]
    fn test_compile_call_prim() {
        do_test_compile(
            r#"
                int_add(2, 3)
            "#,
            expect![[r#"
                Program {
                    reg_count: 1,
                    blocks: [
                        BlockInfo {
                            codes: [
                                call IntAdd(2_int, 3_int),
                                print("it", v_0),
                            ],
                            cont: exit,
                        },
                    ],
                    fns: [],
                    codes: [],
                }"#]],
        );
    }

    #[test]
    fn test_compile_if() {
        do_test_compile(
            r#"
                if false { 1 } else if false { 2 } else { 3 }
            "#,
            expect![[r#"
                Program {
                    reg_count: 3,
                    blocks: [
                        BlockInfo {
                            codes: [],
                            cont: jump (if false then b_1 else b_2),
                        },
                        BlockInfo {
                            codes: [
                                im v_1 <- 1_int,
                            ],
                            cont: jump b_3,
                        },
                        BlockInfo {
                            codes: [],
                            cont: jump (if false then b_4 else b_5),
                        },
                        BlockInfo {
                            codes: [
                                print("it", v_1),
                            ],
                            cont: exit,
                        },
                        BlockInfo {
                            codes: [
                                im v_2 <- 2_int,
                            ],
                            cont: jump b_6,
                        },
                        BlockInfo {
                            codes: [
                                im v_2 <- 3_int,
                            ],
                            cont: jump b_6,
                        },
                        BlockInfo {
                            codes: [
                                mov v_1 <- v_2,
                            ],
                            cont: jump b_3,
                        },
                    ],
                    fns: [],
                    codes: [],
                }"#]],
        );
    }

    #[test]
    #[should_panic(expected = "inconsistent")]
    fn test_compile_if_without_inconsistent_types() {
        do_test_compile(
            r#"
                if true { 1 } else { false }
            "#,
            expect![[""]],
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
                Program {
                    reg_count: 1,
                    blocks: [
                        BlockInfo {
                            codes: [
                                store s_1 <- f_0,
                                print("zero_fn", f_0),
                                call s_1(),
                                print("it", v_0),
                            ],
                            cont: exit,
                        },
                        BlockInfo {
                            codes: [
                                im v_0 <- 0_int,
                            ],
                            cont: ret,
                        },
                    ],
                    fns: [
                        FnInfo {
                            local_var_count: 0,
                            entry_block: 1,
                        },
                    ],
                    codes: [],
                }"#]],
        );
    }

    #[test]
    fn test_compile_fn_with_params() {
        do_test_compile(
            r#"
                let twice = fn(f: fn(int) -> int, x: int) -> int { f(f(x)) };
                twice(fn(x: int) -> int { x }, 0);
            "#,
            expect![[r#"
                Program {
                    reg_count: 1,
                    blocks: [
                        BlockInfo {
                            codes: [
                                store s_3 <- f_0,
                                print("twice", f_0),
                                call s_3(f_1, 0_int),
                                print("it", v_0),
                            ],
                            cont: exit,
                        },
                        BlockInfo {
                            codes: [
                                call p_1(p_2),
                                call p_1(v_0),
                                mov v_0 <- v_0,
                            ],
                            cont: ret,
                        },
                        BlockInfo {
                            codes: [
                                load v_0 <- p_4,
                            ],
                            cont: ret,
                        },
                    ],
                    fns: [
                        FnInfo {
                            local_var_count: 2,
                            entry_block: 1,
                        },
                        FnInfo {
                            local_var_count: 1,
                            entry_block: 2,
                        },
                    ],
                    codes: [],
                }"#]],
        );
    }

    #[should_panic(expected = "unknown var inner")]
    #[test]
    fn test_compile_scope_error() {
        do_test_compile(
            r#"
                {
                    let inner = 1;
                }
                inner;
            "#,
            expect![[""]],
        )
    }
}
