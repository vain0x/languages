pub mod cmd;
pub mod gen_mir;
pub mod parse;
pub mod sema;
pub mod tokenize;

use crate::cmd::*;
use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;
use std::str;

const OPS: &[(&str, Op, OpLevel)] = &[
    ("=", Op::Set, OpLevel::Set),
    ("==", Op::Eq, OpLevel::Cmp),
    ("+", Op::Add, OpLevel::Add),
    ("-", Op::Sub, OpLevel::Add),
    ("*", Op::Mul, OpLevel::Mul),
    ("/", Op::Div, OpLevel::Mul),
    ("%", Op::Mod, OpLevel::Mul),
];

const PUNS: &'static [&'static str] = &["(", ")", "[", "]", "{", "}", ",", ";"];

#[derive(Clone, Copy, Debug)]
pub enum PrimArity {
    Fixed(usize),
    Infinite,
    Bin,
}

const GLOBAL_FUN_ID: FunId = FunId(0);

const NO_REG_ID: RegId = RegId(0);
const BASE_PTR_REG_ID: RegId = RegId(1);
#[allow(unused)]
const STACK_PTR_REG_ID: RegId = RegId(2);
const RET_REG_ID: RegId = RegId(3);
const KNOWN_REG_NUM: usize = 4;
const REG_NUM: usize = 12;

type Span = (usize, usize);
type Pos = (usize, usize);
type Range = (Pos, Pos);

macro_rules! define_rich_id {
    ($($name:ident),*) => {
        $(
            #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
            pub struct $name(usize);

            impl From<usize> for $name {
                fn from(id: usize) -> Self {
                    $name(id)
                }
            }

            impl From<$name> for usize {
                fn from(id: $name) -> usize {
                    id.0
                }
            }

            impl std::ops::Add<usize> for $name {
                type Output = Self;

                fn add(self, rhs: usize) -> Self {
                    $name(self.0 + rhs)
                }
            }

            impl std::ops::AddAssign<usize> for $name {
                fn add_assign(&mut self, rhs: usize)  {
                    self.0 += rhs;
                }
            }

            impl std::ops::Sub<usize> for $name {
                type Output = Self;

                fn sub(self, rhs: usize) -> Self {
                    $name(self.0 - min(self.0, rhs))
                }
            }
        )*
    };
}

define_rich_id!(MsgId, TokenId, ExpId, SymbolId, RegId, LabelId, VarId, FunId);

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Op {
    Set,
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum OpLevel {
    Set,
    Cmp,
    Add,
    Mul,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Keyword {
    Let,
    Def,
    If,
    Else,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenKind {
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
pub struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Clone, Debug)]
pub struct Msg {
    level: MsgLevel,
    message: String,
    exp_id: ExpId,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MsgLevel {
    Err,
}

/// Expression in concrete syntax tree.
#[derive(Clone, PartialEq, Debug)]
pub enum ExpKind {
    Err(MsgId),
    Int(i64),
    Str(String),
    Ident(String),
    Call {
        callee: ExpId,
        args: Vec<ExpId>,
    },
    Bin {
        op: Op,
        l: ExpId,
        r: ExpId,
    },
    If {
        cond: ExpId,
        body: ExpId,
        alt: ExpId,
    },
    Let {
        pat: ExpId,
        init: ExpId,
    },
    Semi(Vec<ExpId>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Exp {
    kind: ExpKind,
    span: Span,
}

pub trait ExpVisitor {
    type Output;

    fn on_err(&mut self, exp_id: ExpId, msg_id: MsgId) -> Self::Output;
    fn on_int(&mut self, exp_id: ExpId, value: i64) -> Self::Output;
    fn on_str(&mut self, exp_id: ExpId, value: &str) -> Self::Output;
    fn on_call(&mut self, exp_id: ExpId, callee: ExpId, args: &[ExpId]) -> Self::Output;
    fn on_bin(&mut self, exp_id: ExpId, op: Op, l: ExpId, r: ExpId) -> Self::Output;
    fn on_let(&mut self, exp_id: ExpId, pat: ExpId, init: ExpId) -> Self::Output;
    fn on_semi(&mut self, exp_id: ExpId, exps: &[ExpId]) -> Self::Output;
}

#[derive(Clone, Debug)]
pub struct Syntax {
    src: String,
    tokens: BTreeMap<TokenId, Token>,
    exps: BTreeMap<ExpId, Exp>,
    root_exp_id: ExpId,
    msgs: BTreeMap<MsgId, Msg>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Prim {
    ReadInt,
    PrintLnInt,
}

#[derive(Clone, PartialEq, Debug)]
pub enum CallKind {
    Prim(Prim),
}

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolKind {
    Prim(Prim),
    Local { index: usize },
}

#[derive(Clone, PartialEq, Debug)]
pub struct Symbol {
    kind: SymbolKind,
    name: String,
}

#[derive(Clone, Debug)]
pub struct FunDef {
    pub name: String,
    pub body: ExpId,
    pub locals: Vec<SymbolId>,
}

#[derive(Clone, Debug)]
pub struct Sema {
    pub syntax: Rc<Syntax>,
    pub pats: BTreeSet<ExpId>,
    pub symbols: BTreeMap<SymbolId, Symbol>,
    pub exp_symbols: BTreeMap<ExpId, SymbolId>,
    pub funs: BTreeMap<FunId, FunDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

#[derive(Clone, Copy, Debug)]
pub enum CmdArg {
    None,
    Int(i64),
    Reg(RegId),
    Label(LabelId),
}

pub type Ins = (Cmd, RegId, CmdArg);

#[derive(Clone, Debug)]
pub struct GenFunDef {
    pub label_id: LabelId,
    pub inss: Vec<Ins>,
}

#[derive(Clone, Debug)]
pub struct Mir {
    pub sema: Rc<Sema>,
    pub reg_count: usize,
    pub label_count: usize,
    pub funs: BTreeMap<FunId, GenFunDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

impl OpLevel {
    fn next_level(self) -> Option<Self> {
        match self {
            OpLevel::Set => Some(OpLevel::Cmp),
            OpLevel::Cmp => Some(OpLevel::Add),
            OpLevel::Add => Some(OpLevel::Mul),
            OpLevel::Mul => None,
        }
    }

    fn contains(self, the_op: Op) -> bool {
        for &(_, op, op_level) in OPS {
            if op == the_op && op_level == self {
                return true;
            }
        }
        false
    }
}

impl Keyword {
    fn text(self) -> &'static str {
        match self {
            Keyword::Let => "let",
            Keyword::Def => "def",
            Keyword::If => "if",
            Keyword::Else => "else",
        }
    }

    fn get_all() -> &'static [Keyword] {
        &[Keyword::Let, Keyword::Def, Keyword::If, Keyword::Else]
    }
}

impl Msg {
    pub fn err(message: String, exp_id: ExpId) -> Self {
        Msg {
            level: MsgLevel::Err,
            message,
            exp_id,
        }
    }
}

impl Syntax {
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

    pub fn locate_exp(&self, exp_id: ExpId) -> Range {
        let (l, r) = self.exps[&exp_id].span;
        let l_pos = self.locate(l);
        let r_pos = self.locate(r);
        (l_pos, r_pos)
    }
}

#[derive(Clone)]
pub struct CompilationResult {
    pub success: bool,
    pub program: String,
    pub stderr: String,
}

pub fn compile(src: &str) -> CompilationResult {
    let src = src.to_owned();
    let syntax = Rc::new(parse::parse(src));
    let sema = Rc::new(sema::sema(syntax));
    gen_mir::gen_mir(sema)
}
