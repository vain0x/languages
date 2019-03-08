use crate::*;

pub(crate) type Span = (usize, usize);
pub(crate) type Pos = (usize, usize);
pub(crate) type Range = (Pos, Pos);

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Op {
    Set,
    SetAdd,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum OpLevel {
    Set,
    Cmp,
    Add,
    Mul,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Keyword {
    Let,
    Fun,
    If,
    Else,
    While,
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

#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Expression in concrete syntax tree.
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ExpKind {
    Err(MsgId),
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

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Exp {
    pub kind: ExpKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub(crate) struct Syntax {
    pub src: String,
    pub tokens: BTreeMap<TokenId, Token>,
    pub exps: BTreeMap<ExpId, Exp>,
    pub root_exp_id: ExpId,
    pub msgs: BTreeMap<MsgId, Msg>,
}

pub(crate) trait ShareSyntax {
    fn share_syntax(&self) -> Rc<Syntax>;
}

pub(crate) const OPS: &[(&str, Op, OpLevel)] = &[
    ("=", Op::Set, OpLevel::Set),
    ("+=", Op::SetAdd, OpLevel::Set),
    ("==", Op::Eq, OpLevel::Cmp),
    ("!=", Op::Ne, OpLevel::Cmp),
    ("<", Op::Lt, OpLevel::Cmp),
    ("<=", Op::Le, OpLevel::Cmp),
    (">", Op::Gt, OpLevel::Cmp),
    (">=", Op::Ge, OpLevel::Cmp),
    ("+", Op::Add, OpLevel::Add),
    ("-", Op::Sub, OpLevel::Add),
    ("*", Op::Mul, OpLevel::Mul),
    ("/", Op::Div, OpLevel::Mul),
    ("%", Op::Mod, OpLevel::Mul),
];

pub(crate) const PUNS: &'static [&'static str] = &["(", ")", "[", "]", "{", "}", ",", ";"];

impl OpLevel {
    pub(crate) fn next_level(self) -> Option<Self> {
        match self {
            OpLevel::Set => Some(OpLevel::Cmp),
            OpLevel::Cmp => Some(OpLevel::Add),
            OpLevel::Add => Some(OpLevel::Mul),
            OpLevel::Mul => None,
        }
    }

    pub(crate) fn contains(self, the_op: Op) -> bool {
        for &(_, op, op_level) in OPS {
            if op == the_op && op_level == self {
                return true;
            }
        }
        false
    }
}

impl Keyword {
    pub(crate) fn text(self) -> &'static str {
        match self {
            Keyword::Let => "let",
            Keyword::Fun => "fun",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
        }
    }

    pub(crate) fn get_all() -> &'static [Keyword] {
        &[
            Keyword::Let,
            Keyword::Fun,
            Keyword::If,
            Keyword::Else,
            Keyword::While,
        ]
    }
}

impl Syntax {
    pub(crate) fn locate(&self, x: usize) -> Pos {
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

    pub(crate) fn locate_exp(&self, exp_id: ExpId) -> Range {
        let (l, r) = self.exps[&exp_id].span;
        let l_pos = self.locate(l);
        let r_pos = self.locate(r);
        (l_pos, r_pos)
    }
}
