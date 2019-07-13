use crate::syntax::tokenize::tokenize;
use crate::syntax::*;
use al_aux::syntax::*;
use std::rc::Rc;

type Parser<'a> = TokenParser<'a, Token>;

// 具象構文木のノード
struct SyntaxNode {
    start_loc: SourceLocation,
}

trait ParserExt {
    // 具象構文木のノードの開始時に呼ばれる。
    // ノードの範囲の開始位置を記録する。
    fn start(&self) -> SyntaxNode {
        SyntaxNode {
            start_loc: self.loc(),
        }
    }

    fn text(&self) -> &str;

    fn loc(&self) -> SourceLocation;
}

impl<'a> ParserExt for TokenParser<'a, Token> {
    fn text(&self) -> &str {
        self.next_token().text()
    }

    fn loc(&self) -> SourceLocation {
        self.next_token().loc()
    }
}

impl SyntaxNode {
    // 具象構文木のノードの終了時に呼ばれる。
    // ノードの範囲を計算して AST ノードに保持させる。
    // この範囲にはカッコのような抽象構文木が持たない情報も含まれる。
    fn finish(&self, mut ast: Ast, p: &mut Parser<'_>) -> Ast {
        let total_loc = match p.prev_token() {
            Some(t) => self.start_loc.union(&t.loc()),
            None => self.start_loc,
        };

        ast.extend_loc(&total_loc);
        ast
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
            let group = p.start();
            p.bump();
            let body = parse_term(p);
            if !p.at(TokenKind::ParenR) {
                panic!("no )")
            }
            p.bump();
            group.finish(body, p)
        }
        _ => {
            panic!("expected an expression");
        }
    }
}

fn parse_call(p: &mut Parser<'_>) -> Ast {
    let call = p.start();
    let cal = parse_atom(p);

    if !p.at(TokenKind::ParenL) {
        return cal;
    }

    let loc = p.loc();
    let arg = parse_atom(p);

    call.finish(Ast::new(AstKind::Call, vec![cal, arg], loc), p)
}

fn parse_mul(p: &mut Parser<'_>) -> Ast {
    let mul = p.start();
    let left = parse_call(p);

    if p.at(TokenKind::Star) {
        let loc = p.loc();
        p.bump();

        let right = parse_call(p);
        return mul.finish(Ast::new(AstKind::BinOp(BinOp::Mul), vec![left, right], loc), p);
    }

    if p.at(TokenKind::Slash) {
        let loc = p.loc();
        p.bump();

        let right = parse_call(p);
        return mul.finish(Ast::new(AstKind::BinOp(BinOp::Div), vec![left, right], loc), p);
    }

    left
}

fn parse_add(p: &mut Parser<'_>) -> Ast {
    let add = p.start();
    let left = parse_mul(p);

    if p.at(TokenKind::Plus) {
        let loc = p.loc();
        p.bump();

        let right = parse_mul(p);
        return add.finish(Ast::new(AstKind::BinOp(BinOp::Add), vec![left, right], loc), p);
    }

    if p.at(TokenKind::Minus) {
        let loc = p.loc();
        p.bump();

        let right = parse_mul(p);
        return add.finish(Ast::new(AstKind::BinOp(BinOp::Sub), vec![left, right], loc), p);
    }

    left
}

fn parse_eq(p: &mut Parser<'_>) -> Ast {
    let eq = p.start();
    let left = parse_add(p);

    if !p.at(TokenKind::EqEq) {
        return left;
    }
    let loc = p.loc();
    p.bump();

    let right = parse_add(p);
    eq.finish(Ast::new(AstKind::BinOp(BinOp::Eq), vec![left, right], loc), p)
}

fn parse_term(p: &mut Parser<'_>) -> Ast {
    parse_eq(p)
}

fn parse_semi(loc: SourceLocation, p: &mut Parser<'_>) -> Ast {
    let semi = p.start();
    let mut children = vec![];

    while !p.at_eof() {
        let child = parse_term(p);
        children.push(child);
    }

    semi.finish(Ast::new(AstKind::Semi, children, loc), p)
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
