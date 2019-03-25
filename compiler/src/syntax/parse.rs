use super::*;
use std::cell::RefCell;

struct Parser<'a> {
    syntax: &'a mut Syntax,
    module_id: ModuleId,
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

        assert!(l <= r, "{:?}..{:?}", l, r);
        let r1 = if l == r { l } else { r - 1 };
        let span = (
            self.syntax.tokens[&l].span.0,
            self.syntax.tokens[&r1].span.1,
        );

        let module_id = self.module_id;
        let exp_id = self.syntax.exps.len().into();
        self.syntax.exps.insert(
            exp_id,
            Exp {
                kind,
                module_id,
                span,
            },
        );
        exp_id
    }

    fn add_exp_err(&mut self, kind: SyntaxError, token_span: (TokenId, TokenId)) -> ExpId {
        self.add_exp(ExpKind::Err(kind), token_span)
    }

    fn text(&self, token_id: TokenId) -> &str {
        self.syntax.token_text(token_id)
    }

    fn parse_err(&mut self, kind: SyntaxError) -> ExpId {
        self.add_exp_err(kind, (self.current, self.current + 1))
    }

    fn parse_atom(&mut self) -> ExpId {
        let token_l = self.current;

        match self.next().kind {
            TokenKind::Int => {
                self.current += 1;
                let value = self.text(token_l).parse::<i64>().ok().unwrap_or(-1);
                self.add_exp(ExpKind::Int(value), (token_l, self.current))
            }
            TokenKind::Char => {
                let token_l = self.current;
                self.current += 1;

                let text = self.text(token_l);
                match parse_char(text) {
                    Err(err) => self.add_exp_err(err, (token_l, self.current)),
                    Ok(c) => self.add_exp(ExpKind::Byte(c), (token_l, self.current)),
                }
            }
            TokenKind::Str => {
                self.current += 1;

                let text = self.text(token_l);
                match parse_str(text) {
                    Err(err) => self.add_exp_err(err, (token_l, self.current)),
                    Ok(value) => self.add_exp(ExpKind::Str(value), (token_l, self.current)),
                }
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
                    return self.parse_err(SyntaxError::ExpectedChar(')'));
                }
                self.current += 1;
                exp_id
            }
            TokenKind::Pun("{") => {
                self.current += 1;
                let exp_id = self.parse_exp();

                if self.next().kind != TokenKind::Pun("}") {
                    return self.parse_err(SyntaxError::ExpectedChar('}'));
                }
                self.current += 1;
                exp_id
            }
            TokenKind::Err => {
                self.current += 1;
                self.parse_err(SyntaxError::InvalidChar)
            }
            TokenKind::Eof => self.add_exp_err(SyntaxError::UnexpectedEof, (token_l, self.current)),
            _ => {
                self.current += 1;
                self.add_exp_err(SyntaxError::ExpectedExp, (token_l, self.current))
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
                        return self.parse_err(SyntaxError::ExpectedChar(')'));
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
                        return self.parse_err(SyntaxError::ExpectedChar(']'));
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
        match self.next().kind {
            TokenKind::Op(Op::Sub) => {
                self.current += 1;
                let l = self.add_exp(ExpKind::Int(0), (token_l, self.current));
                let r = self.parse_prefix();
                self.add_exp(ExpKind::Bin { op: Op::Sub, l, r }, (token_l, self.current))
            }
            // No parameter lambda.
            TokenKind::Op(Op::LogOr) => {
                self.current += 1;
                let body = self.parse_term();
                self.add_exp(ExpKind::Fun { pats: vec![], body }, (token_l, self.current))
            }
            // Lambda.
            TokenKind::Op(Op::BitOr) => {
                self.current += 1;

                let mut pats = vec![];
                if self.next().kind != TokenKind::Op(Op::BitOr) {
                    self.parse_atom_list(&mut pats);
                }

                if self.next().kind != TokenKind::Op(Op::BitOr) {
                    return self.parse_err(SyntaxError::ExpectedChar('|'));
                }
                self.current += 1;

                let body = self.parse_term();

                self.add_exp(ExpKind::Fun { pats, body }, (token_l, self.current))
            }
            _ => self.parse_suffix(),
        }
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
            return self.parse_err(SyntaxError::ExpectedChar('{'));
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
            _ => self.parse_err(SyntaxError::ExpectedEither(vec![
                "}".to_string(),
                "if".to_string(),
            ])),
        };

        self.add_exp(ExpKind::If { cond, body, alt }, (token_l, self.current))
    }

    fn parse_while(&mut self) -> ExpId {
        let token_l = self.current;
        self.current += 1;

        let cond = self.parse_term();

        if self.next().kind != TokenKind::Pun("{") {
            return self.parse_err(SyntaxError::ExpectedChar('{'));
        }
        let body = self.parse_term();

        self.add_exp(ExpKind::While { cond, body }, (token_l, self.current))
    }

    fn parse_break(&mut self) -> ExpId {
        self.current += 1;

        self.add_exp(ExpKind::Break, (self.current - 1, self.current))
    }

    fn parse_continue(&mut self) -> ExpId {
        self.current += 1;

        self.add_exp(ExpKind::Continue, (self.current - 1, self.current))
    }

    fn parse_term(&mut self) -> ExpId {
        match self.next().kind {
            TokenKind::Keyword(Keyword::Return) => self.parse_return(),
            TokenKind::Keyword(Keyword::If) => self.parse_if(),
            TokenKind::Keyword(Keyword::While) => self.parse_while(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break(),
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

        let rec = if self.next().kind == TokenKind::Keyword(Keyword::Rec) {
            self.current += 1;
            true
        } else {
            false
        };

        let pat_exp_id = self.parse_atom();

        if self.next().kind != TokenKind::Op(Op::Set) {
            return self.parse_err(SyntaxError::ExpectedChar('='));
        }
        self.current += 1;

        let init_exp_id = self.parse_term();

        self.add_exp(
            ExpKind::Let {
                pat: pat_exp_id,
                init: init_exp_id,
                rec,
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

    fn parse_atom_list(&mut self, items: &mut Vec<ExpId>) {
        while self.is_followed_by_term() {
            items.push(self.parse_atom());

            match self.next().kind {
                TokenKind::Pun(",") => {
                    self.current += 1;
                    continue;
                }
                _ => break,
            }
        }
    }

    fn parse_eof(&mut self, exp_id: ExpId) -> ExpId {
        if self.next().kind != TokenKind::Eof {
            let exp_r = self.parse_err(SyntaxError::ExpectedEof);
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

/// Parse char literal.
fn parse_char(text: &str) -> Result<u8, SyntaxError> {
    if !(text.len() >= 2 && text.starts_with('\'') && text.ends_with('\'')) {
        return Err(SyntaxError::MissingSingleQuote);
    }

    let body = text[1..text.len() - 1].as_bytes();
    let c = match body {
        b"\\0" => b'\0',
        b"\\n" => b'\n',
        b"\\r" => b'\r',
        b"\\t" => b'\t',
        b"\\\\" | b"\\'" | b"\\\"" => body[1],
        _ if body.len() >= 1 && body[0] == b'\\' => return Err(SyntaxError::UnknownEscapeSequence),
        _ if body.len() == 1 => body[0],
        _ => return Err(SyntaxError::NonSingleCharLiteral),
    };
    Ok(c)
}

// Parse string literal.
fn parse_str(text: &str) -> Result<String, SyntaxError> {
    if !(text.len() >= 2 && text.starts_with('\"') && text.ends_with('\"')) {
        return Err(SyntaxError::MissingDoubleQuote);
    }

    let body = &text[1..text.len() - 1];

    // FIXME: Handle escape sequences correctly.
    let body = body.replace("\\n", "\n");
    if body.contains('\\') {
        return Err(SyntaxError::Unimplemented(
            "Escape sequences other than '\\n' are not supported yet",
        ));
    }

    Ok(body)
}

pub(crate) fn parse(syntax: &'_ mut Syntax, module_id: ModuleId, root_token_id: TokenId) -> ExpId {
    let mut parser = Parser {
        syntax,
        module_id,
        first: root_token_id,
        current: root_token_id,
        tick: RefCell::new(0),
    };
    parser.parse()
}
