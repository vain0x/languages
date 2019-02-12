pub mod gen_mir;
pub mod parse;
pub mod regalloc;
pub mod sema;
pub mod tokenize;

use std::collections::BTreeMap;
use std::rc::Rc;
use std::str;

macro_rules! define_op {
    ($($op:ident,)*) => {
        #[derive(Clone, Copy, PartialEq, Debug)]
        pub enum Op {
            $($op),*
        }

        pub fn serialize_op(op: Op) -> &'static str {
            $(if op == Op::$op {
                return stringify!($op);
            })*
            unreachable!()
        }
    };
}

define_op! {
    Kill,
    Imm,
    AddImm,
    Mov,
    Store,
    Load,
    Push,
    Pop,
    Label,
    Jump,
    Unless,
    Call,
    Ret,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    ToStr,
    StrCat,
    ReadInt,
    ReadStr,
    Print,
    PrintLn,
    Exit,
}

const PUNS: &'static [&'static str] = &["(", ")"];

#[derive(Clone, Copy, Debug)]
pub enum PrimArity {
    Fixed(usize),
    Infinite,
    Bin,
}

const EOF: &'static Token = &Token::Eof;

const GLOBAL_FUN_ID: FunId = 0;

const NO_REG_ID: RegId = 0;
const BASE_PTR_REG_ID: RegId = 1;
#[allow(unused)]
const STACK_PTR_REG_ID: RegId = 2;
const RET_REG_ID: RegId = 3;
const KNOWN_REG_NUM: usize = 4;
const REG_NUM: usize = 12;

type TokenId = usize;
type Span = (usize, usize);
type Pos = (usize, usize);
type Range = (Pos, Pos);
type NodeId = usize;
type ExpId = usize;
type StrId = usize;
type RegId = usize;
type LabelId = usize;
type VarId = usize;
type FunId = usize;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Err(String),
    Id(String),
    Int(i64),
    Str(String),
    Pun(&'static str),
    Eof,
}

/// Node in concrete syntax tree.
#[derive(Clone, PartialEq, Debug)]
pub enum Node {
    Err(String, TokenId),
    Value(TokenId),
    App(Vec<NodeId>),
}

#[derive(Clone, Debug, Default)]
pub struct Syntax {
    src: String,
    tokens: Vec<Token>,
    token_spans: Vec<Span>,
    nodes: Vec<Node>,
    node_spans: Vec<Span>,
}

#[derive(Clone, Debug)]
pub struct Msg {
    level: MsgLevel,
    message: String,
    node_id: NodeId,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MsgLevel {
    Err,
}

impl Msg {
    pub fn err(message: String, node_id: NodeId) -> Self {
        Msg {
            level: MsgLevel::Err,
            message,
            node_id,
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

    pub fn locate_node(&self, node_id: NodeId) -> Range {
        let (l, r) = self.node_spans[node_id];
        let l_pos = self.locate(l);
        let r_pos = self.locate(r);
        (l_pos, r_pos)
    }
}

pub fn compile(src: &str) -> (bool, String, String) {
    let src = src.to_owned();
    let syntax = Rc::new(parse::parse(src));
    let sema = Rc::new(sema::sema(syntax));
    gen_mir::gen_mir(sema)
}
