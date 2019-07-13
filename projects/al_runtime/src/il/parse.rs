use al_aux::il::*;
use al_aux::syntax::*;

type P<'a> = TokenParser<'a, IlToken>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum IlTokenKind {
    Error,
    Eof,
    Trivia,
    Int,

    /// `$foo`
    Ident,

    Atom,
    ParenL,
    ParenR,
}

#[derive(Clone, Debug)]
pub(crate) struct IlToken {
    kind: IlTokenKind,
    start: usize,
    end: usize,
}

pub(crate) struct IlTokenFactory;

impl IlToken {
    fn kind(&self) -> IlTokenKind {
        self.kind
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl TokenKindTrait for IlTokenKind {
    fn error() -> Self {
        IlTokenKind::Error
    }

    fn eof() -> Self {
        IlTokenKind::Eof
    }

    fn is_trivia(&self) -> bool {
        *self == IlTokenKind::Trivia
    }
}

impl TokenTrait for IlToken {
    type Kind = IlTokenKind;

    fn kind(&self) -> IlTokenKind {
        self.kind()
    }
}

impl TokenFactoryTrait for IlTokenFactory {
    type Token = IlToken;

    type Kind = IlTokenKind;

    fn new_token(&self, kind: Self::Kind, start: usize, end: usize) -> Self::Token {
        IlToken { kind, start, end }
    }
}

static ATOM_KINDS: &[(&str, IlKind)] = &[
    ("root", IlKind::Root),
    ("code_section", IlKind::CodeSection),
    ("globals", IlKind::Globals),
    ("semi", IlKind::Semi),
    ("assert", IlKind::Assert),
    ("cell_set", IlKind::CellSet),
    ("false", IlKind::Bool(false)),
    ("true", IlKind::Bool(true)),
    ("global_get", IlKind::GlobalGet),
    ("+", IlKind::OpAdd),
    ("-", IlKind::OpSub),
    ("*", IlKind::OpMul),
    ("/", IlKind::OpDiv),
    ("==", IlKind::OpEq),
];

fn tokenize(text: &str) -> Vec<IlToken> {
    Tokenizer::new(IlTokenFactory, text).tokenize(|t| {
        if t.eat_while(|c| c.is_ascii_whitespace()) {
            t.add_token(IlTokenKind::Trivia);
        }

        if t.eat("//") {
            t.eat_while(|c| c != b'\n');
            t.add_token(IlTokenKind::Trivia);
        }

        if t.eat("(") {
            t.add_token(IlTokenKind::ParenL);
        }

        if t.eat(")") {
            t.add_token(IlTokenKind::ParenR);
        }

        if t.eat("$") {
            t.eat_while(|c| c.is_ascii_alphanumeric() || c == b'_');
            t.add_token(IlTokenKind::Ident);
        }

        if t.eat_while(|c| c.is_ascii_digit()) {
            t.add_token(IlTokenKind::Int);
        }

        if t.eat_while(|c| !c.is_ascii_whitespace() && c != b')') {
            t.add_token(IlTokenKind::Atom);
        }
    })
}

fn next_text<'a>(text: &'a str, p: &mut P<'_>) -> &'a str {
    let token = p.next_token();
    let (start, end) = (token.start(), token.end());
    &text[start..end]
}

fn parse_atom(text: &str, t: &mut IlTree, p: &mut P<'_>) -> IlKind {
    match p.next() {
        IlTokenKind::Atom => {
            let x = next_text(text, p);
            for &(y, kind) in ATOM_KINDS {
                if x == y {
                    p.bump();
                    return kind;
                }
            }
            panic!("Unknown atom {}", x);
        }
        IlTokenKind::Int => {
            let value = next_text(text, p).parse::<i64>().unwrap();
            p.bump();
            IlKind::Int(value)
        }
        IlTokenKind::Ident => {
            let ident = next_text(text, p).to_owned();
            let string_id = t.add_string(ident);
            p.bump();
            IlKind::Ident(string_id)
        }
        _ => panic!("Expected atom but '{}'", next_text(text, p)),
    }
}

fn parse_term(text: &str, t: &mut IlTree, p: &mut P<'_>) -> usize {
    if !p.at(IlTokenKind::ParenL) {
        let kind = parse_atom(text, t, p);
        return t.add_leaf(kind);
    }

    p.bump();
    let kind = parse_atom(text, t, p);

    let mut children = vec![];
    while !p.at_eof() && !p.at(IlTokenKind::ParenR) {
        children.push(parse_term(text, t, p));
    }

    assert!(p.at(IlTokenKind::ParenR));
    p.bump();

    t.add_node(kind, &children)
}

fn parse_root(text: &str, t: &mut IlTree, p: &mut P<'_>) -> usize {
    let body = parse_term(text, t, p);

    if !p.at_eof() {
        panic!("expected EOF");
    }

    body
}

pub(crate) fn parse(text: &str) -> IlTree {
    let tokens = tokenize(text);
    let mut parser = TokenParser::new(&tokens);
    let mut tree = IlTree::new();
    let root = parse_root(text, &mut tree, &mut parser);
    tree.set_root(root);
    tree
}
