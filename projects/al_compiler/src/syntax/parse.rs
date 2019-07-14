//! 構文解析 (parse)
//!
//! 字句解析で生成したトークン列を解析して、式の構造を分析する。
//! 結果として抽象構文木を生成する。

use crate::syntax::tokenize::tokenize;
use crate::syntax::*;
use al_aux::syntax::*;
use std::rc::Rc;

type Parser<'a> = TokenParser<'a, Token>;

/// 具象構文木のノード
struct SyntaxNode {
    start_loc: SourceLocation,
}

trait ParserExt {
    /// 具象構文木のノードの開始時に呼ばれる。
    /// ノードの範囲の開始位置を記録する。
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
    /// 具象構文木のノードの終了時に呼ばれる。
    /// ノードの範囲を計算して AST ノードに保持させる。
    /// この範囲にはカッコのような抽象構文木が持たない情報も含まれる。
    fn finish(&self, mut ast: Ast, p: &mut Parser<'_>) -> Ast {
        let total_loc = match p.prev_token() {
            Some(t) => self.start_loc.union(&t.loc()),
            None => self.start_loc,
        };

        ast.extend_loc(&total_loc);
        ast
    }
}

static BIN_OP_TABLE: &[(BinOp, TokenKind)] = &[
    (BinOp::Assign, TokenKind::Eq),
    (BinOp::Add, TokenKind::Plus),
    (BinOp::Sub, TokenKind::Minus),
    (BinOp::Mul, TokenKind::Star),
    (BinOp::Div, TokenKind::Slash),
    (BinOp::Eq, TokenKind::EqEq),
];

/// ブロック式 `{ x; y; ..; z }`
fn parse_block(p: &mut Parser<'_>) -> Ast {
    let block = p.start();

    assert_eq!(p.next(), TokenKind::BraceL);
    let loc = p.loc();
    p.bump();

    let body = parse_semi(loc, TokenKind::BraceR, p);

    if !p.at(TokenKind::BraceR) {
        panic!("missing }")
    }
    p.bump();

    block.finish(body, p)
}

/// アトム式。トークン1個か、カッコで構成される種類の式。
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
        TokenKind::BraceL => parse_block(p),
        _ => {
            panic!("expected an expression");
        }
    }
}

/// 関数呼び出し式
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

/// 二項演算式の左辺
fn parse_bin_left(level: BinOpLevel, p: &mut Parser<'_>) -> Ast {
    match level.next() {
        None => parse_call(p),
        Some(next_level) => parse_bin(next_level, p),
    }
}

/// 二項演算式の右辺
fn parse_bin_right(level: BinOpLevel, p: &mut Parser<'_>) -> Ast {
    // NOTE: 右結合のケースがある
    parse_bin_left(level, p)
}

/// 特定のレベルの二項演算式をパースする。
fn parse_bin(level: BinOpLevel, p: &mut Parser<'_>) -> Ast {
    let bins = p.start();
    let mut left = parse_bin_left(level, p);

    loop {
        // 次の位置のトークンが対応する二項演算子を調べる。
        let bin_op_opt = BIN_OP_TABLE
            .iter()
            .filter_map(|&(bin_op, token_kind)| {
                if bin_op.level() == level && p.at(token_kind) {
                    Some(bin_op)
                } else {
                    None
                }
            })
            .next();

        if let Some(bin_op) = bin_op_opt {
            let op_loc = p.loc();
            p.bump();

            let right = parse_bin_right(level, p);
            let ast = bins.finish(Ast::new(AstKind::Bin(bin_op), vec![left, right], op_loc), p);
            left = ast;
            continue;
        }

        return left;
    }
}

/// 最も結合度の弱い二項演算子の式
fn parse_bin_top(p: &mut Parser<'_>) -> Ast {
    parse_bin(BinOpLevel::first(), p)
}

fn parse_term(p: &mut Parser<'_>) -> Ast {
    parse_bin_top(p)
}

/// 項の並び。
/// 一般的な言語の「セミコロンで区切られた文の並び」に相当するものなので、semi と呼ぶ。
fn parse_semi(loc: SourceLocation, end_kind: TokenKind, p: &mut Parser<'_>) -> Ast {
    let semi = p.start();
    let mut children = vec![];

    while !p.at_eof() && !p.at(end_kind) {
        let child = parse_term(p);
        children.push(child);
    }

    semi.finish(Ast::new(AstKind::Semi, children, loc), p)
}

/// ソースコード全体をパースする。
fn parse_root(p: &mut Parser<'_>) -> Ast {
    let root = p.start();

    let loc = p.loc();
    let body = parse_semi(loc, TokenKind::Eof, p);

    if !p.at_eof() {
        panic!("expected EOF {:?}", p.loc())
    }

    root.finish(body, p)
}

pub(crate) fn parse(file: usize, text: &str) -> Rc<Ast> {
    let tokens = tokenize(file, text);
    let mut p = Parser::new(&tokens);
    let ast = parse_root(&mut p);
    Rc::new(ast)
}
