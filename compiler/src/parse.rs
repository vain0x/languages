use crate::*;

struct Parser<'a> {
    src: &'a str,
    tokens: &'a BTreeMap<TokenId, Token>,
    current: TokenId,
    exps: BTreeMap<ExpId, Exp>,
    msgs: BTreeMap<MsgId, Msg>,
}

impl Parser<'_> {
    fn next(&self) -> &Token {
        assert!(self.current < TokenId(self.tokens.len()));
        &self.tokens[&self.current]
    }

    fn is_followed_by_exp(&self) -> bool {
        match self.next().kind {
            TokenKind::Err
            | TokenKind::Eof
            | TokenKind::Pun(")")
            | TokenKind::Pun("]")
            | TokenKind::Pun("}")
            | TokenKind::Pun(",") => false,
            _ => true,
        }
    }

    fn is_followed_by_term(&self) -> bool {
        match self.next().kind {
            _ if !self.is_followed_by_exp() => false,
            TokenKind::Pun(";") => false,
            _ => true,
        }
    }

    fn add_exp(&mut self, kind: ExpKind, token_span: (TokenId, TokenId)) -> ExpId {
        let (l, r) = token_span;

        assert!(l < self.current, "l={:?} r={:?}", l, r);
        let r1 = if l == r { l } else { r - 1 };
        let span = (self.tokens[&l].span.0, self.tokens[&r1].span.1);

        let exp_id = self.exps.len().into();
        self.exps.insert(exp_id, Exp { kind, span });
        exp_id
    }

    fn add_exp_err(&mut self, message: String, token_span: (TokenId, TokenId)) -> ExpId {
        let msg_id = self.msgs.len().into();
        let exp_id = self.add_exp(ExpKind::Err(msg_id), token_span);
        self.msgs.insert(msg_id, Msg::err(message, exp_id));
        exp_id
    }

    fn text(&self, token_id: TokenId) -> &str {
        let token = self.tokens[&token_id];
        &self.src[token.span.0..token.span.1]
    }

    fn parse_err(&mut self, message: String) -> ExpId {
        self.add_exp_err(message, (self.current, self.current))
    }

    fn parse_atom(&mut self) -> ExpId {
        let token_l = self.current;

        match self.next().kind {
            TokenKind::Int => {
                self.current += 1;
                let value = self.text(token_l).parse::<i64>().ok().unwrap_or(-1);
                self.add_exp(ExpKind::Int(value), (token_l, self.current))
            }
            TokenKind::Str => {
                self.current += 1;
                let value = {
                    let text = self.text(token_l);
                    text[1..text.len() - 1].to_string()
                };
                self.add_exp(ExpKind::Str(value), (token_l, self.current))
            }
            TokenKind::Ident => {
                self.current += 1;
                let name = self.text(token_l).to_string();
                self.add_exp(ExpKind::Ident(name), (token_l, self.current))
            }
            TokenKind::Pun("(") => {
                self.current += 1;
                let exp_id = self.parse_term();

                if self.next().kind != TokenKind::Pun(")") {
                    return self.parse_err("Expected ')'".to_string());
                }
                self.current += 1;
                exp_id
            }
            TokenKind::Err => {
                self.current += 1;
                self.parse_err("Invalid character".to_string())
            }
            TokenKind::Eof => {
                self.add_exp_err("Unexpected EOF".to_string(), (token_l, self.current))
            }
            _ => {
                self.current += 1;
                self.add_exp_err(
                    "Expected an expression".to_string(),
                    (token_l, self.current),
                )
            }
        }
    }

    fn parse_suffix(&mut self) -> ExpId {
        let token_l = self.current;
        let mut exp_l = self.parse_atom();

        while self.next().kind == TokenKind::Pun("(") {
            self.current += 1;

            let mut args = vec![];
            self.parse_term_list(&mut args);

            if self.next().kind != TokenKind::Pun(")") {
                return self.parse_err("Expected ')'".to_string());
            }

            exp_l = self.add_exp(
                ExpKind::Call {
                    callee: exp_l,
                    args,
                },
                (token_l, self.current),
            );
        }

        exp_l
    }

    fn parse_bin_next(&mut self, op_level: OpLevel) -> ExpId {
        match op_level.next_level() {
            None => self.parse_suffix(),
            Some(op_level) => self.parse_bin_l(op_level),
        }
    }

    fn parse_bin_l(&mut self, op_level: OpLevel) -> ExpId {
        let mut exp_l = self.parse_bin_next(op_level);
        let token_l = self.current;

        while let &Token {
            kind: TokenKind::Op(op),
            ..
        } = self.next()
        {
            if !op_level.contains(op) {
                break;
            }

            self.current += 1;

            let exp_r = self.parse_bin_next(op_level);
            exp_l = self.add_exp(
                ExpKind::Bin {
                    op,
                    l: exp_l,
                    r: exp_r,
                },
                (token_l, self.current),
            );
        }

        exp_l
    }

    fn parse_term(&mut self) -> ExpId {
        self.parse_bin_l(OpLevel::Set)
    }

    fn parse_term_list(&mut self, items: &mut Vec<ExpId>) {
        while self.is_followed_by_term() {
            items.push(self.parse_term());

            match self.next().kind {
                TokenKind::Pun(",") => {
                    self.current += 1;
                    continue;
                }
                _ => break,
            }
        }
    }

    fn parse_let(&mut self) -> ExpId {
        let token_l = self.current;
        self.current += 1;

        let pat_exp_id = self.parse_atom();

        if self.next().kind != TokenKind::Op(Op::Set) {
            return self.parse_err("Expected '='".to_string());
        }
        self.current += 1;

        let init_exp_id = self.parse_term();

        self.add_exp(
            ExpKind::Let {
                pat: pat_exp_id,
                init: init_exp_id,
            },
            (token_l, self.current),
        )
    }

    fn parse_def(&mut self) -> ExpId {
        unimplemented!()
    }

    fn parse_stmt(&mut self) -> ExpId {
        match self.next().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let(),
            TokenKind::Keyword(Keyword::Def) => self.parse_def(),
            _ => self.parse_term(),
        }
    }

    fn parse_exp(&mut self) -> ExpId {
        let token_l = self.current;
        let mut children = vec![];

        while self.is_followed_by_exp() {
            children.push(self.parse_stmt());
        }

        let token_span = (token_l, self.current);
        match children.len() {
            0 => self.add_exp(ExpKind::Semi(vec![]), token_span),
            1 => children[0],
            _ => self.add_exp(ExpKind::Semi(children), token_span),
        }
    }

    fn parse(&mut self) -> ExpId {
        self.parse_exp()
    }
}

pub fn parse(src: String) -> Syntax {
    let tokens = tokenize::tokenize(&src);

    let (root_exp_id, exps, msgs) = {
        let mut parser = Parser {
            src: &src,
            tokens: &tokens,
            current: TokenId(0),
            exps: BTreeMap::new(),
            msgs: BTreeMap::new(),
        };
        let root_exp_id = parser.parse();
        (root_exp_id, parser.exps, parser.msgs)
    };

    Syntax {
        src,
        tokens,
        exps,
        root_exp_id,
        msgs,
    }
}
