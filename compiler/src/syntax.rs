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

pub(crate) type DocId = Id<Doc>;
pub(crate) type ModuleId = Id<Module>;
pub(crate) type TokenId = Id<TokenTag>;
pub(crate) type ExpId = Id<ExpTag>;

#[derive(Clone, Debug)]
pub struct Doc {
    doc_id: DocId,
    uri: String,
    src: Rc<String>,
}

#[derive(Clone, Debug)]
pub struct Module {
    doc: Rc<Doc>,
    root_token_id: TokenId,
    root_exp_id: ExpId,
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
    Char(u8),
    Str,
}

#[derive(Clone, Debug)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub module_id: ModuleId,
    pub span: Span,
}

/// Expression in concrete syntax tree.
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ExpKind {
    Err(String),
    Unit,
    Int(i64),
    Byte(u8),
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
    Return(ExpId),
    If {
        cond: ExpId,
        body: ExpId,
        alt: ExpId,
    },
    While {
        cond: ExpId,
        body: ExpId,
    },
    Break,
    Continue,
    Let {
        pat: ExpId,
        init: ExpId,
        rec: bool,
    },
    Semi(Vec<ExpId>),
}

#[derive(Clone, Debug)]
pub(crate) struct Exp {
    pub kind: ExpKind,
    pub module_id: ModuleId,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Syntax {
    pub modules: BTreeMap<ModuleId, Module>,
    pub tokens: BTreeMap<TokenId, Token>,
    pub exps: BTreeMap<ExpId, Exp>,
}

pub(crate) trait ShareSyntax {
    fn share_syntax(&self) -> Rc<Syntax>;
}

pub(crate) const PUNS: &'static [&'static str] = &["(", ")", "[", "]", "{", "}", ",", ";"];

impl Doc {
    pub(crate) fn new(doc_id: DocId, uri: String, src: Rc<String>) -> Self {
        Doc { doc_id, uri, src }
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

impl Syntax {
    pub(crate) fn token_text(&self, token_id: TokenId) -> &str {
        let token = &self.tokens[&token_id];
        let (l, r) = token.span;
        let doc = &self.modules[&token.module_id].doc;
        &doc.src[l..r]
    }

    pub(crate) fn add_module(&mut self, doc: Rc<Doc>) {
        let module_id = ModuleId::new(self.modules.len());
        self.modules.insert(
            module_id,
            Module {
                doc: Rc::clone(&doc),
                root_token_id: TokenId::new(0),
                root_exp_id: ExpId::new(0),
            },
        );

        let root_token_id = tokenize::tokenize(self, module_id, Rc::clone(&doc));
        let root_exp_id = parse::parse(self, module_id, root_token_id);

        self.modules.entry(module_id).and_modify(|module| {
            module.root_token_id = root_token_id;
            module.root_exp_id = root_exp_id;
        });
    }

    pub(crate) fn module_root_exps<'a>(&'a self) -> impl Iterator<Item = ExpId> + 'a {
        self.modules
            .values()
            .map(|module: &'a _| module.root_exp_id)
    }

    pub(crate) fn locate_exp(&self, exp_id: ExpId) -> Range {
        let exp = &self.exps[&exp_id];
        let doc = &self.modules[&exp.module_id].doc;
        let (l, r) = exp.span;
        let l_pos = doc.locate(l);
        let r_pos = doc.locate(r);
        (l_pos, r_pos)
    }

    pub fn exp_kind(&self, exp_id: ExpId) -> &ExpKind {
        &self.exps[&exp_id].kind
    }
}
