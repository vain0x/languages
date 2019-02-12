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
    tokens: Vec<Token>,
    spans: Vec<Span>,
    nodes: Vec<Node>,
}

pub fn compile(src: &str) -> String {
    let src = src.to_owned();
    let syntax = Rc::new(parse::parse(src));
    let sema = Rc::new(sema::sema(syntax));
    gen_mir::gen_mir(sema)
}
