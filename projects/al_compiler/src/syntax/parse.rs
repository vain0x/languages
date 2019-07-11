use crate::syntax::tokenize::tokenize;
use crate::syntax::*;
use al_aux::syntax::TokenKindTrait;
use std::cell::Cell;
use std::rc::Rc;

type Parser<'a> = TokenParser<'a, Token>;

trait TokenTrait {
    type Kind: TokenKindTrait + PartialEq;

    fn kind(&self) -> Self::Kind;
}

struct TokenParser<'a, Token> {
    tokens: &'a [Token],
    current: usize,
    tick: Cell<usize>,
}

impl<'a, Token: TokenTrait> TokenParser<'a, Token> {
    fn new(tokens: &'a [Token]) -> Self {
        TokenParser {
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

    fn next_token(&self) -> &Token {
        self.detect_infinite_loop();

        &self.tokens[self.current]
    }

    fn next(&self) -> Token::Kind {
        self.next_token().kind()
    }

    fn at_eof(&self) -> bool {
        Token::Kind::eof().into_iter().any(|kind| self.at(kind))
    }

    fn at(&self, kind: Token::Kind) -> bool {
        self.next() == kind
    }
}

trait ParserExt {
    fn text(&self) -> &str;
    fn loc(&self) -> SourceLocation;
}

impl TokenTrait for Token {
    type Kind = TokenKind;

    fn kind(&self) -> TokenKind {
        self.kind()
    }
}

impl<'a> ParserExt for TokenParser<'a, Token> {
    fn text(&self) -> &str {
        self.next_token().text()
    }

    fn loc(&self) -> SourceLocation {
        self.next_token().loc()
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

fn parse_mul(p: &mut Parser<'_>) -> Ast {
    let left = parse_call(p);

    if p.at(TokenKind::Star) {
        let loc = p.loc();
        p.bump();

        let right = parse_call(p);
        return Ast::new(AstKind::Mul, vec![left, right], loc);
    }

    if p.at(TokenKind::Slash) {
        let loc = p.loc();
        p.bump();

        let right = parse_call(p);
        return Ast::new(AstKind::Div, vec![left, right], loc);
    }

    left
}

fn parse_add(p: &mut Parser<'_>) -> Ast {
    let left = parse_mul(p);

    if p.at(TokenKind::Plus) {
        let loc = p.loc();
        p.bump();

        let right = parse_mul(p);
        return Ast::new(AstKind::Add, vec![left, right], loc);
    }

    if p.at(TokenKind::Minus) {
        let loc = p.loc();
        p.bump();

        let right = parse_mul(p);
        return Ast::new(AstKind::Sub, vec![left, right], loc);
    }

    left
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
