use crate::{
    ast::*,
    tokenize::{Token, Tokenizer},
};
use text_position_rs::CompositePosition;

/// Parser context.
pub(crate) struct Px<'b> {
    tokens: Tokenizer<'b>,
    last: Option<(Token, &'b str, CompositePosition)>,
}

impl<'b> Px<'b> {
    pub(crate) fn new(mut tokens: Tokenizer<'b>) -> Self {
        let last = tokens.next();
        Px { tokens, last }
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.last.is_none()
    }

    pub(crate) fn next(&self) -> Token {
        match self.last {
            Some((token, _, _)) => token,
            None => Token::RightBrace,
        }
    }

    pub(crate) fn bump(&mut self) -> (Token, &'b str, CompositePosition) {
        assert!(self.last.is_some());

        let token = self.last.unwrap();
        self.last = self.tokens.next();
        token
    }

    pub(crate) fn eat(&mut self, kind: Token) -> bool {
        if self.next() == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    pub(crate) fn finish(self) {
        assert!(self.last.is_none(), "Expected EOF");
    }
}

fn parse_atom_expr(px: &mut Px<'_>) -> AExpr {
    match px.next() {
        Token::Ident => {
            let (_, ident, _) = px.bump();
            AExpr {
                kind: AExprKind::Symbol(AIdent(ident.to_string())),
            }
        }
        Token::DecimalInt => {
            let (_, ident, _) = px.bump();
            AExpr {
                kind: AExprKind::Lit(ALit::DecimalInt(ident.parse().expect("parse"))),
            }
        }
        Token::String => {
            let (_, ident, _) = px.bump();
            AExpr {
                kind: AExprKind::Lit(ALit::String(ident[1..ident.len() - 1].to_string())),
            }
        }
        _ => panic!("unexpected token, {:?}", px.last),
    }
}

fn parse_call_expr(px: &mut Px<'_>) -> AExpr {
    let callee = parse_atom_expr(px);

    match px.next() {
        Token::LeftParen => {
            px.bump();

            let mut args = vec![];
            loop {
                if px.eat(Token::RightParen) {
                    px.bump();
                    break;
                }

                let arg = parse_expr(px);
                args.push(arg);
                px.eat(Token::Comma);
            }
            AExpr {
                kind: AExprKind::Call {
                    callee: Box::new(callee),
                    args,
                },
            }
        }
        _ => callee,
    }
}

fn parse_expr(px: &mut Px<'_>) -> AExpr {
    parse_call_expr(px)
}

fn parse_root(px: &mut Px<'_>) -> ARoot {
    let mut exprs = vec![];

    while !px.at_eof() {
        exprs.push(parse_expr(px));
        px.eat(Token::Semi);
    }

    ARoot(exprs)
}

pub fn parse_tokens<'b>(tokens: Tokenizer<'b>) -> ARoot {
    let mut px = Px::new(tokens);
    let root = parse_root(&mut px);
    px.finish();
    root
}
