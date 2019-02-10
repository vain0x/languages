pub mod gen_mir;
pub mod parse;
pub mod regalloc;
pub mod sema;
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

#[derive(Clone, Copy, Debug)]
pub enum PrimArity {
    Fixed(usize),
    Infinite,
    Bin,
}

const EOF: &'static Tok = &Tok::Eof;

const GLOBAL_FUN_ID: FunId = 0;

const NO_REG_ID: RegId = 0;
const BASE_PTR_REG_ID: RegId = 1;
#[allow(unused)]
const STACK_PTR_REG_ID: RegId = 2;
const RET_REG_ID: RegId = 3;
const KNOWN_REG_NUM: usize = 4;
const REG_NUM: usize = 12;

type TokId = usize;
type Range = (usize, usize);
type SynId = usize;
type ExpId = usize;
type StrId = usize;
type RegId = usize;
type LabId = usize;
type VarId = usize;
type FunId = usize;

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
pub struct Syntax {
    toks: Vec<Tok>,
    syns: Vec<Syn>,
}

pub trait HaveSyntaxModel {
    fn toks(&self) -> &[Tok];
    fn syns(&self) -> &[Syn];

    fn syn_as_id(&self, syn_id: SynId) -> Option<&str> {
        match &self.syns()[syn_id] {
            &Syn::Val(tok_id) => match &self.toks()[tok_id] {
                Tok::Id(name) => Some(name),
                _ => None,
            },
            _ => None,
        }
    }

    fn syn_as_app(&self, syn_id: SynId) -> Option<&[SynId]> {
        match &self.syns()[syn_id] {
            Syn::App(syns) => Some(&syns),
            _ => None,
        }
    }
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

    let mut sema = sema::Sema {
        toks: toks.to_owned(),
        syns: syns.to_owned(),
        ..sema::Sema::default()
    };
    sema.sema();

    gen_mir::Compiler {
        toks: toks,
        syns: syns,
        sema_vars: sema.vars,
        sema_funs: sema.funs,
        sema_exps: sema.exps,
        ..gen_mir::Compiler::default()
    }
    .compile()
}
