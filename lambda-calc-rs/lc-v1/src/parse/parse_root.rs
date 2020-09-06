use super::parser::{LambdaParser, LambdaParserHost};
use crate::token::token_kind::TokenKind;
use lc_utils::parser::Parser;

impl<'a, H: LambdaParserHost<'a>> LambdaParser<'a, H> {
    pub(crate) fn parse_root(&mut self) -> H::AfterRoot {
        let mut decls = vec![];
        loop {
            if let TokenKind::Eof = self.next() {
                break;
            }

            let decl = match self.parse_decl() {
                Some(it) => it,
                None => {
                    eprintln!("expected decl");
                    self.skip();
                    continue;
                }
            };

            decls.push(decl);
        }

        let eof = self.bump();
        self.host.after_root(decls, eof)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::tokenize_rules::MyTokenizerHost;
    use crate::{
        ast::a_parser::AstLambdaParserHost, context::Context, parse::parser::LambdaParser,
    };
    use expect_test::{expect, Expect};
    use lc_utils::deque_chan::deque_chan;
    use lc_utils::tokenizer::Tokenizer;
    use std::collections::VecDeque;

    fn do_test_parse(source_code: &str, expect: Expect) {
        let context = Context::new();
        let mut tokens = VecDeque::new();

        let (tx, rx) = deque_chan(&mut tokens);
        let mut tokenizer_host = MyTokenizerHost::new(tx);
        let tokenizer = Tokenizer::new(source_code, &mut tokenizer_host);

        let mut parser_host = AstLambdaParserHost::new(&context);
        let mut parser = LambdaParser::new(source_code, tokenizer, rx, &mut parser_host);
        let tokens = parser.parse_root();

        let actual = format!("{:#?}", tokens);
        expect.assert_eq(&actual);
    }

    #[test]
    fn test_parse_let_decls() {
        let source_code = "let id = id; let x = 1;";
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
        do_test_parse(source_code, expect);
    }

    #[test]
    fn test_call_expr() {
        do_test_parse(
            r#"
                let x0 = f0();
                let x1 = f1(x);
                let x2 = f2(x, y);
            "#,
            expect![[r#"
                ARoot {
                    decls: [
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "x0",
                                ),
                                init_opt: Some(
                                    Call(
                                        ACallExpr {
                                            callee: Var(
                                                "f0",
                                            ),
                                            args: [],
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "x1",
                                ),
                                init_opt: Some(
                                    Call(
                                        ACallExpr {
                                            callee: Var(
                                                "f1",
                                            ),
                                            args: [
                                                Var(
                                                    "x",
                                                ),
                                            ],
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "x2",
                                ),
                                init_opt: Some(
                                    Call(
                                        ACallExpr {
                                            callee: Var(
                                                "f2",
                                            ),
                                            args: [
                                                Var(
                                                    "x",
                                                ),
                                                Var(
                                                    "y",
                                                ),
                                            ],
                                        },
                                    ),
                                ),
                            },
                        ),
                    ],
                    eof: Eof,
                }"#]],
        );
    }

    #[test]
    fn test_paren_expr() {
        do_test_parse(
            r#"
                let _ = (f)(x);
                let _ = (fn(x)(x))(x);
                let _ = ();
            "#,
            expect![[r#"
                ARoot {
                    decls: [
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "_",
                                ),
                                init_opt: Some(
                                    Call(
                                        ACallExpr {
                                            callee: Var(
                                                "f",
                                            ),
                                            args: [
                                                Var(
                                                    "x",
                                                ),
                                            ],
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "_",
                                ),
                                init_opt: Some(
                                    Call(
                                        ACallExpr {
                                            callee: Fn(
                                                AFnExpr {
                                                    params: [
                                                        (
                                                            "x",
                                                            None,
                                                        ),
                                                    ],
                                                    body_opt: Some(
                                                        Var(
                                                            "x",
                                                        ),
                                                    ),
                                                },
                                            ),
                                            args: [
                                                Var(
                                                    "x",
                                                ),
                                            ],
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "_",
                                ),
                                init_opt: None,
                            },
                        ),
                    ],
                    eof: Eof,
                }"#]],
        );
    }

    #[test]
    fn test_call_expr_chain() {
        do_test_parse(
            r#"
                let _ = f(1)(2)(3);
            "#,
            expect![[r#"
                ARoot {
                    decls: [
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "_",
                                ),
                                init_opt: Some(
                                    Call(
                                        ACallExpr {
                                            callee: Call(
                                                ACallExpr {
                                                    callee: Call(
                                                        ACallExpr {
                                                            callee: Var(
                                                                "f",
                                                            ),
                                                            args: [
                                                                Number(
                                                                    "1",
                                                                ),
                                                            ],
                                                        },
                                                    ),
                                                    args: [
                                                        Number(
                                                            "2",
                                                        ),
                                                    ],
                                                },
                                            ),
                                            args: [
                                                Number(
                                                    "3",
                                                ),
                                            ],
                                        },
                                    ),
                                ),
                            },
                        ),
                    ],
                    eof: Eof,
                }"#]],
        );
    }

    #[test]
    fn test_fn_expr() {
        do_test_parse(
            r#"
                let zero_fn = fn() 0;
                let church_two = fn(f, x) f(f(x));
                let id = fn(x) x;

                let param_list_missing = fn x;
                let body_missing = fn();
            "#,
            expect![[r#"
                ARoot {
                    decls: [
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "zero_fn",
                                ),
                                init_opt: Some(
                                    Fn(
                                        AFnExpr {
                                            params: [],
                                            body_opt: Some(
                                                Number(
                                                    "0",
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "church_two",
                                ),
                                init_opt: Some(
                                    Fn(
                                        AFnExpr {
                                            params: [
                                                (
                                                    "f",
                                                    None,
                                                ),
                                                (
                                                    "x",
                                                    None,
                                                ),
                                            ],
                                            body_opt: Some(
                                                Call(
                                                    ACallExpr {
                                                        callee: Var(
                                                            "f",
                                                        ),
                                                        args: [
                                                            Call(
                                                                ACallExpr {
                                                                    callee: Var(
                                                                        "f",
                                                                    ),
                                                                    args: [
                                                                        Var(
                                                                            "x",
                                                                        ),
                                                                    ],
                                                                },
                                                            ),
                                                        ],
                                                    },
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "id",
                                ),
                                init_opt: Some(
                                    Fn(
                                        AFnExpr {
                                            params: [
                                                (
                                                    "x",
                                                    None,
                                                ),
                                            ],
                                            body_opt: Some(
                                                Var(
                                                    "x",
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "param_list_missing",
                                ),
                                init_opt: Some(
                                    Fn(
                                        AFnExpr {
                                            params: [],
                                            body_opt: Some(
                                                Var(
                                                    "x",
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            },
                        ),
                        Let(
                            ALetDecl {
                                name_opt: Some(
                                    "body_missing",
                                ),
                                init_opt: Some(
                                    Fn(
                                        AFnExpr {
                                            params: [],
                                            body_opt: None,
                                        },
                                    ),
                                ),
                            },
                        ),
                    ],
                    eof: Eof,
                }"#]],
        );
    }
}
