use crate::*;
use std::cell::RefCell;

struct Parser<'a> {
    tokens: &'a BTreeMap<TokenId, Token>,
    current: TokenId,
    exps: BTreeMap<ExpId, Exp>,
    msgs: BTreeMap<MsgId, Msg>,
    tick: RefCell<usize>,
}

impl Parser<'_> {
    fn next(&self) -> &Token {
        debug_assert!({
            *self.tick.borrow_mut() += 1;
            *self.tick.borrow() < 1_000_000
        });

        assert!(self.current < TokenId::new(self.tokens.len()));
        &self.tokens[&self.current]
    }

    fn is_followed_by_exp(&self) -> bool {
        match self.next().kind {
            TokenKind::Err
            | TokenKind::Eof
            | TokenKind::Keyword(Keyword::Else)
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

        assert!(l <= r, "{:?}..{:?}", l, r);
        let r1 = if l == r { l } else { r - 1 };
        let span = (self.tokens[&l].span.0, self.tokens[&r1].span.1);

        let exp_id = self.exps.len().into();
        self.exps.insert(exp_id, Exp { kind, span });
        exp_id
    }

    fn add_exp_err(&mut self, message: String, token_span: (TokenId, TokenId)) -> ExpId {
        let msg_id = self.next_msg_id();
        let exp_id = self.add_exp(ExpKind::Err(msg_id), token_span);
        self.add_err_msg(message, exp_id);
        exp_id
    }

    fn text(&self, token_id: TokenId) -> &str {
        self.tokens[&token_id].text()
    }

    fn parse_err(&mut self, message: String) -> ExpId {
        self.add_exp_err(message, (self.current, self.current + 1))
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
                    text[1..text.len() - 1].replace("\\n", "\n").to_string()
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
            TokenKind::Pun("{") => {
                self.current += 1;
                let exp_id = self.parse_exp();

                if self.next().kind != TokenKind::Pun("}") {
                    return self.parse_err("Expected '}'".to_string());
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

        loop {
            match self.next().kind {
                TokenKind::Pun("(") => {
                    self.current += 1;

                    let mut args = vec![];
                    self.parse_term_list(&mut args);

                    if self.next().kind != TokenKind::Pun(")") {
                        return self.parse_err("Expected ')'".to_string());
                    }
                    self.current += 1;

                    exp_l = self.add_exp(
                        ExpKind::Call {
                            callee: exp_l,
                            args,
                        },
                        (token_l, self.current),
                    );
                }
                TokenKind::Pun("[") => {
                    self.current += 1;

                    let arg_exp_id = self.parse_term();

                    if self.next().kind != TokenKind::Pun("]") {
                        return self.parse_err("Expected ']'".to_string());
                    }
                    self.current += 1;

                    exp_l = self.add_exp(
                        ExpKind::Index {
                            indexee: exp_l,
                            arg: arg_exp_id,
                        },
                        (token_l, self.current),
                    );
                }
                _ => break,
            }
        }

        exp_l
    }

    fn parse_prefix(&mut self) -> ExpId {
        let token_l = self.current;
        if self.next().kind == TokenKind::Op(Op::Sub) {
            self.current += 1;
            let l = self.add_exp(ExpKind::Int(0), (token_l, self.current));
            let r = self.parse_suffix();
            return self.add_exp(ExpKind::Bin { op: Op::Sub, l, r }, (token_l, self.current));
        }

        self.parse_suffix()
    }

    fn parse_bin_next(&mut self, op_level: OpLevel) -> ExpId {
        match op_level.next_level() {
            None => self.parse_prefix(),
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

    fn parse_fun(&mut self) -> ExpId {
        let token_l = self.current;
        self.current += 1;

        if self.next().kind != TokenKind::Pun("(") {
            return self.parse_err("Expected '('".to_string());
        }
        self.current += 1;

        let mut pats = vec![];
        self.parse_term_list(&mut pats);

        if self.next().kind != TokenKind::Pun(")") {
            return self.parse_err("Expected ')'".to_string());
        }
        self.current += 1;

        let body = self.parse_term();

        self.add_exp(ExpKind::Fun { pats, body }, (token_l, self.current))
    }

    fn parse_if(&mut self) -> ExpId {
        let token_l = self.current;
        self.current += 1;

        let cond = self.parse_term();

        if self.next().kind != TokenKind::Pun("{") {
            return self.parse_err("Expected '{'".to_string());
        }
        let body = self.parse_term();

        if self.next().kind != TokenKind::Keyword(Keyword::Else) {
            let alt = self.add_exp(ExpKind::Semi(vec![]), (token_l, self.current));

            return self.add_exp(ExpKind::If { cond, body, alt }, (token_l, self.current));
        }
        self.current += 1;

        let alt = match self.next().kind {
            TokenKind::Pun("{") => self.parse_atom(),
            TokenKind::Keyword(Keyword::If) => self.parse_term(),
            _ => self.parse_err("Expected '}' or 'if'".to_string()),
        };

        self.add_exp(ExpKind::If { cond, body, alt }, (token_l, self.current))
    }

    fn parse_while(&mut self) -> ExpId {
        let token_l = self.current;
        self.current += 1;

        let cond = self.parse_term();

        if self.next().kind != TokenKind::Pun("{") {
            return self.parse_err("Expected '{'".to_string());
        }
        let body = self.parse_term();

        self.add_exp(ExpKind::While { cond, body }, (token_l, self.current))
    }

    fn parse_term(&mut self) -> ExpId {
        match self.next().kind {
            TokenKind::Keyword(Keyword::Fun) => self.parse_fun(),
            TokenKind::Keyword(Keyword::If) => self.parse_if(),
            TokenKind::Keyword(Keyword::While) => self.parse_while(),
            _ => self.parse_bin_l(OpLevel::Set),
        }
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

    fn parse_stmt(&mut self) -> ExpId {
        match self.next().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let(),
            _ => self.parse_term(),
        }
    }

    fn parse_exp(&mut self) -> ExpId {
        let token_l = self.current;
        let mut children = vec![];

        while self.is_followed_by_exp() {
            children.push(self.parse_stmt());

            while self.next().kind == TokenKind::Pun(";") {
                self.current += 1;
            }
        }

        let token_span = (token_l, self.current);
        match children.len() {
            0 => self.add_exp(ExpKind::Semi(vec![]), token_span),
            1 => children[0],
            _ => self.add_exp(ExpKind::Semi(children), token_span),
        }
    }

    fn parse_eof(&mut self) {
        if self.next().kind != TokenKind::Eof {
            self.parse_err("Expected EOF".to_string());
        }
    }

    fn parse(&mut self) -> ExpId {
        let exp_id = self.parse_exp();
        self.parse_eof();
        exp_id
    }
}

impl BorrowMutMsgs for Parser<'_> {
    fn msgs_mut(&mut self) -> &mut Msgs {
        &mut self.msgs
    }
}

pub(crate) fn parse(doc: Rc<Doc>) -> Syntax {
    let tokens = tokenize::tokenize(Rc::clone(&doc));

    let (root_exp_id, exps, msgs) = {
        let mut parser = Parser {
            tokens: &tokens,
            current: TokenId::default(),
            exps: BTreeMap::new(),
            msgs: BTreeMap::new(),
            tick: RefCell::new(0),
        };
        let root_exp_id = parser.parse();
        (root_exp_id, parser.exps, parser.msgs)
    };

    Syntax {
        doc,
        tokens,
        exps,
        root_exp_id,
        msgs,
    }
}
