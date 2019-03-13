use super::*;
use std::cell::RefCell;
use std::rc::Rc;

struct Parser<'a> {
    syntax: &'a mut Syntax,
    first: TokenId,
    current: TokenId,
    tick: RefCell<usize>,
}

impl Parser<'_> {
    fn next(&self) -> &Token {
        debug_assert!({
            *self.tick.borrow_mut() += 1;
            *self.tick.borrow() < 1_000_000
        });

        assert!(self.current < TokenId::new(self.syntax.tokens.len()));
        &self.syntax.tokens[&self.current]
    }

    fn prev(&self) -> Option<&Token> {
        if self.current == self.first {
            return None;
        }
        Some(&self.syntax.tokens[&(self.current - 1)])
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
        let doc = Rc::clone(&self.syntax.tokens[&l].doc);

        assert!(l <= r, "{:?}..{:?}", l, r);
        let r1 = if l == r { l } else { r - 1 };
        let span = (
            self.syntax.tokens[&l].span.0,
            self.syntax.tokens[&r1].span.1,
        );

        let exp_id = self.syntax.exps.len().into();
        self.syntax.exps.insert(exp_id, Exp { kind, doc, span });
        exp_id
    }

    fn add_exp_err(&mut self, message: String, token_span: (TokenId, TokenId)) -> ExpId {
        self.add_exp(ExpKind::Err(message), token_span)
    }

    fn text(&self, token_id: TokenId) -> &str {
        self.syntax.tokens[&token_id].text()
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
                if self.next().kind == TokenKind::Pun(")") {
                    self.current += 1;
                    return self.add_exp(ExpKind::Unit, (token_l, self.current));
                }

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
        let token_l = self.current;
        let mut exp_l = self.parse_bin_next(op_level);

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

    fn parse_return(&mut self) -> ExpId {
        let token_l = self.current;
        self.current += 1;

        let exp_id = if self.is_followed_by_term() {
            self.parse_term()
        } else {
            self.add_exp(ExpKind::Unit, (token_l, token_l))
        };

        self.add_exp(ExpKind::Return(exp_id), (token_l, self.current))
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
            let alt = self.add_exp(ExpKind::Unit, (token_l, self.current));

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

    fn parse_continue(&mut self) -> ExpId {
        self.current += 1;

        self.add_exp(ExpKind::Continue, (self.current - 1, self.current))
    }

    fn parse_term(&mut self) -> ExpId {
        match self.next().kind {
            TokenKind::Keyword(Keyword::Fun) => self.parse_fun(),
            TokenKind::Keyword(Keyword::Return) => self.parse_return(),
            TokenKind::Keyword(Keyword::If) => self.parse_if(),
            TokenKind::Keyword(Keyword::While) => self.parse_while(),
            TokenKind::Keyword(Keyword::Continue) => self.parse_continue(),
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

        // Trailing semicolons discard the result.
        if let Some(TokenKind::Pun(";")) = self.prev().map(|t| t.kind) {
            children.push(self.add_exp(ExpKind::Unit, (self.current - 1, self.current - 1)));
        }

        let token_span = (token_l, self.current);
        match children.len() {
            0 => self.add_exp(ExpKind::Unit, token_span),
            1 => children[0],
            _ => self.add_exp(ExpKind::Semi(children), token_span),
        }
    }

    fn parse_eof(&mut self, exp_id: ExpId) -> ExpId {
        if self.next().kind != TokenKind::Eof {
            let exp_r = self.parse_err("Expected EOF".to_string());
            return self.add_exp(
                ExpKind::Semi(vec![exp_id, exp_r]),
                (self.current, self.current),
            );
        }
        exp_id
    }

    fn parse(&mut self) -> ExpId {
        let exp_id = self.parse_exp();
        self.parse_eof(exp_id)
    }
}

pub(crate) fn parse(syntax: &'_ mut Syntax, root_token_id: TokenId) {
    let root_exp_id = {
        let mut parser = Parser {
            syntax,
            first: root_token_id,
            current: root_token_id,
            tick: RefCell::new(0),
        };
        parser.parse()
    };
    syntax.roots.push(root_exp_id);
}
