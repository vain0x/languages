
use crate::syntax::tokenize::tokenize;
use crate::syntax::*;
use std::cell::Cell;
use std::rc::Rc;
struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    tick: Cell<usize>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            current: 0,
            tick: Cell::default(),
        }
    }

    fn bump(&mut self) {
        if self.at_eof() {
            return;
        }

        assert!(self.current + 1 < self.tokens.len());
        self.current += 1;
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get() + 1;

        assert!(tick < 10_000_000);
        self.tick.set(tick);
    }

    fn text(&self) -> &str {
        self.tokens[self.current].text()
    }

    fn loc(&self) -> SourceLocation {
        self.tokens[self.current].loc()
    }

    fn next(&self) -> TokenKind {
        self.detect_infinite_loop();

        self.tokens[self.current].kind()
    }

    fn at_eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.next() == kind
    }
}

fn parse_atom(p: &mut Parser<'_>) -> Ast {
    let loc = p.loc();
    let children = vec![];

    match p.next() {
        TokenKind::True => {
            p.bump();
            Ast::new(AstKind::True, children, loc)
        }
        TokenKind::False => {
            p.bump();
            Ast::new(AstKind::False, children, loc)
        }
        TokenKind::Assert => {
            p.bump();
            Ast::new(AstKind::Assert, children, loc)
        }
        TokenKind::Ident => {
            let ident = p.text().to_string();
            p.bump();
            Ast::new(AstKind::Ident(ident), children, loc)
        }
        TokenKind::Int => {
            let value = p.text().parse::<i64>().unwrap();
            p.bump();
            Ast::new(AstKind::Int(value), children, loc)
        }
        TokenKind::ParenL => {
            p.bump();
            let body = parse_term(p);
            if !p.at(TokenKind::ParenR) {
                panic!("no )")
            }
            p.bump();
            body
        }
        _ => {
            panic!("expected an expression");
        }
    }
}

fn parse_call(p: &mut Parser<'_>) -> Ast {
    let cal = parse_atom(p);

    if !p.at(TokenKind::ParenL) {
        return cal;
    }

    let loc = p.loc();
    let arg = parse_atom(p);
    Ast::new(AstKind::Call, vec![cal, arg], loc)
}

fn parse_add(p: &mut Parser<'_>) -> Ast {
    let left = parse_call(p);

    if !p.at(TokenKind::Plus) {
        return left;
    }
    let loc = p.loc();
    p.bump();

    let right = parse_call(p);
    Ast::new(AstKind::Add, vec![left, right], loc)
}

fn parse_eq(p: &mut Parser<'_>) -> Ast {
    let left = parse_add(p);

    if !p.at(TokenKind::EqEq) {
        return left;
    }
    let loc = p.loc();
    p.bump();

    let right = parse_add(p);
    Ast::new(AstKind::Eq, vec![left, right], loc)
}

fn parse_term(p: &mut Parser<'_>) -> Ast {
    parse_eq(p)
}

fn parse_semi(loc: SourceLocation, p: &mut Parser<'_>) -> Ast {
    let mut children = vec![];

    while !p.at_eof() {
        let child = parse_term(p);
        children.push(child);
    }

    Ast::new(AstKind::Semi, children, loc)
}

fn parse_root(p: &mut Parser<'_>) -> Ast {
    let loc = p.loc();
    let body = parse_semi(loc, p);

    if !p.at_eof() {
        panic!("expected EOF {:?}", p.loc())
    }

    body
}

pub(crate) fn parse(file: usize, text: &str) -> Rc<Ast> {
    let tokens = tokenize(file, text);
    let mut p = Parser::new(&tokens);
    let ast = parse_root(&mut p);
    Rc::new(ast)
}
