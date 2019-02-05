pub mod gen_mir;
pub mod parse;
pub mod regalloc;
pub mod tokenize;

use std::collections::BTreeMap;
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
const BINS: &'static [(&'static str, Op)] = &[
    ("++", Op::StrCat),
    ("+", Op::Add),
    ("-", Op::Sub),
    ("*", Op::Mul),
    ("/", Op::Div),
    ("%", Op::Mod),
];

const RELS: &'static [(&'static str, Op)] = &[
    ("==", Op::Eq),
    ("!=", Op::Ne),
    ("<", Op::Lt),
    ("<=", Op::Le),
    (">", Op::Gt),
    (">=", Op::Ge),
];

const EOF: &'static Tok = &Tok::Eof;

const GLOBAL_FUN_ID: FunId = 0;

const NO_REG_ID: RegId = 0;
const BASE_PTR_REG_ID: RegId = 1;
const STACK_PTR_REG_ID: RegId = 2;
const RET_REG_ID: RegId = 3;
const KNOWN_REG_NUM: usize = 4;
const REG_NUM: usize = 12;

type TokId = usize;
type Range = (usize, usize);
type SynId = usize;
type RegId = usize;
type LabId = usize;
type FunId = usize;

/// Offset from base ptr.
type Offset = i64;

type VarId = usize;

#[derive(Clone, PartialEq, Debug)]
pub enum Tok {
    Err(String),
    Id(String),
    Int(i64),
    Str(String),
    Pun(&'static str),
    Eof,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Syn {
    Err(String, TokId),
    Val(TokId),
    App(Vec<SynId>),
}

#[derive(Clone, Debug, Default)]
pub struct Var {
    name: String,
    offset: Offset,
    local: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum Val {
    None,
    Int(i64),
    Str(usize),
    Reg(RegId),
    Lab(LabId),
}

type Ins = (Op, RegId, Val);

#[derive(Clone, Default)]
struct Fun {
    name: String,
    lab_id: LabId,
    vars: BTreeMap<String, VarId>,
    var_num: usize,
    ins: Vec<Ins>,
}

#[derive(Clone, Default)]
pub struct Mir {
    funs: Vec<Fun>,
    strs: Vec<String>,
    reg_num: usize,
    lab_num: usize,
}

pub fn compile(src: &str) -> String {
    let src = src.to_owned();

    let mut tokenizer = tokenize::Tokenizer {
        src: src.clone(),
        toks: vec![],
        tok_ranges: vec![],
        cur: 0,
    };
    tokenizer.tokenize();
    let toks = tokenizer.toks;

    let syns = parse::Parser {
        toks: toks.clone(),
        cur: 0,
        syns: vec![],
    }
    .parse();

    gen_mir::Compiler {
        toks: toks,
        syns: syns,
        ..gen_mir::Compiler::default()
    }
    .compile()
}
