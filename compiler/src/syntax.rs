pub(crate) mod keyword;
pub(crate) mod op;
pub(crate) mod parse;
pub(crate) mod tokenize;

pub(crate) use self::keyword::*;
pub(crate) use self::op::*;

use crate::Id;
use std::collections::BTreeMap;
use std::rc::Rc;

pub(crate) type Span = (usize, usize);
pub(crate) type Pos = (usize, usize);
pub(crate) type Range = (Pos, Pos);

pub(crate) struct TokenTag;
pub(crate) struct ExpTag;

pub(crate) type TokenId = Id<TokenTag>;
pub(crate) type ExpId = Id<ExpTag>;

#[derive(Clone, Debug)]
pub struct Doc {
    uri: String,
    src: String,
}

pub(crate) trait BorrowDoc {
    fn doc(&self) -> &Doc;

    fn src(&self) -> &str {
        self.doc().src()
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum TokenKind {
    Err,
    Eof,
    Pun(&'static str),
    Op(Op),
    Keyword(Keyword),
    Ident,
    Int,
    Str,
}

#[derive(Clone, Debug)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub doc: Rc<Doc>,
    pub span: Span,
}

/// Expression in concrete syntax tree.
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ExpKind {
    Err(String),
    Int(i64),
    Str(String),
    Ident(String),
    Call {
        callee: ExpId,
        args: Vec<ExpId>,
    },
    Index {
        indexee: ExpId,
        arg: ExpId,
    },
    Bin {
        op: Op,
        l: ExpId,
        r: ExpId,
    },
    Fun {
        pats: Vec<ExpId>,
        body: ExpId,
    },
    If {
        cond: ExpId,
        body: ExpId,
        alt: ExpId,
    },
    While {
        cond: ExpId,
        body: ExpId,
    },
    Let {
        pat: ExpId,
        init: ExpId,
    },
    Semi(Vec<ExpId>),
}

#[derive(Clone, Debug)]
pub(crate) struct Exp {
    pub kind: ExpKind,
    pub doc: Rc<Doc>,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Syntax {
    pub tokens: BTreeMap<TokenId, Token>,
    pub exps: BTreeMap<ExpId, Exp>,
    pub roots: Vec<ExpId>,
}

pub(crate) trait ShareSyntax {
    fn share_syntax(&self) -> Rc<Syntax>;
}

pub(crate) const PUNS: &'static [&'static str] = &["(", ")", "[", "]", "{", "}", ",", ";"];

impl Doc {
    pub fn new(uri: String, src: String) -> Self {
        Doc { uri, src }
    }

    pub fn uri(&self) -> &str {
        &self.uri
    }

    pub fn src(&self) -> &str {
        &self.src
    }

    fn locate(&self, x: usize) -> Pos {
        let mut line = 0;
        let mut column = 0;
        for &c in &self.src.as_bytes()[0..x] {
            if c == b'\n' {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }
        (line, column)
    }
}

impl Token {
    pub fn text(&self) -> &str {
        let (l, r) = self.span;
        &self.doc.src()[l..r]
    }
}

impl Syntax {
    pub fn add_doc(&mut self, doc: Rc<Doc>) {
        let root_token_id = tokenize::tokenize(self, doc);
        parse::parse(self, root_token_id);
    }

    pub(crate) fn locate_exp(&self, exp_id: ExpId) -> Range {
        let exp = &self.exps[&exp_id];
        let (l, r) = exp.span;
        let l_pos = exp.doc.locate(l);
        let r_pos = exp.doc.locate(r);
        (l_pos, r_pos)
    }

    pub fn exp_kind(&self, exp_id: ExpId) -> &ExpKind {
        &self.exps[&exp_id].kind
    }
}
