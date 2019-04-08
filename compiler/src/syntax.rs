pub(crate) mod error;
pub(crate) mod exp;
pub(crate) mod op;
pub(crate) mod parse;
pub(crate) mod pun;
pub(crate) mod tokenize;

pub(crate) use self::error::*;
pub(crate) use self::exp::*;
pub(crate) use self::op::*;
pub(crate) use self::pun::*;

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
    Pun(Pun),
    Ident,
    Int,
    Char,
    Str,
}

#[derive(Clone, Debug)]
pub(crate) struct Token {
    pub kind: TokenKind,
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

    /// Convert (line, column) to byte index in the source code.
    pub(crate) fn unlocate(&self, mut line: usize, column: usize) -> usize {
        let src = self.src().as_bytes();
        let mut i = 0;
        while i < src.len() && line > 0 {
            if src[i] == b'\n' {
                line -= 1;
            }
            i += 1;
        }

        // FIXME: The result is incorrect if the line contains non-ASCII chars.
        i += column;

        i
    }
}

impl Module {
    fn doc_id(&self) -> DocId {
        self.doc.doc_id
    }

    pub(crate) fn doc(&self) -> &Doc {
        &self.doc
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

    pub(crate) fn find_module_by_doc_id(&self, doc_id: DocId) -> Option<(ModuleId, &Module)> {
        self.modules
            .iter()
            .filter(|&(_, module)| module.doc_id() == doc_id)
            .map(|(&module_id, module)| (module_id, module))
            .next()
    }

    pub(crate) fn locate_exp(&self, exp_id: ExpId) -> Range {
        let exp = &self.exps[&exp_id];
        let doc = &self.modules[&exp.module_id].doc;
        let (l, r) = exp.span;
        let l_pos = doc.locate(l);
        let r_pos = doc.locate(r);
        (l_pos, r_pos)
    }

    /// Find a token that touches the span.
    pub(crate) fn touch_token(&self, module_id: ModuleId, span: Span) -> Option<TokenId> {
        let (sl, sr) = span;
        let mut token_id = self.modules[&module_id].root_token_id;
        while self.tokens[&token_id].kind != TokenKind::Eof {
            let (tl, tr) = self.tokens[&token_id].span;
            if sl < tr && tl < sr {
                return Some(token_id);
            }
            token_id += 1;
        }
        None
    }

    /// Find an expression that is lowest among from exprsesions
    /// that touches the specified span.
    pub(crate) fn touch_lowest(&self, module_id: ModuleId, span: Span) -> ExpId {
        fn dfs(it: &Syntax, exp_id: ExpId, span: Span) -> Option<ExpId> {
            let (sl, sr) = span;
            let (xl, xr) = it.exps[&exp_id].span;
            if xr <= sl || sr <= xl {
                return None;
            }

            Some(
                (it.exps[&exp_id].kind.children().into_iter())
                    .filter_map(|child| dfs(it, child, span))
                    .next()
                    .unwrap_or(exp_id),
            )
        }

        let module = &self.modules[&module_id];
        dfs(self, module.root_exp_id, span).unwrap_or(module.root_exp_id)
    }

    pub fn exp_kind(&self, exp_id: ExpId) -> &ExpKind {
        &self.exps[&exp_id].kind
    }
}
