pub mod cmd;
pub mod gen_mir;
pub mod parse;
pub mod regalloc;
pub mod sema;
pub mod tokenize;

use crate::cmd::*;
use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::iter;
use std::rc::Rc;
use std::str;

const OPS: &[(&str, Op, OpLevel)] = &[
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
    While,
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
    Index {
        indexee: ExpId,
        arg: ExpId,
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
pub struct Exp {
    kind: ExpKind,
    span: Span,
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
    ByteToInt,
    ReadInt,
    PrintLnInt,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Ty {
    Err,
    Var(ExpId),
    Unit,
    Byte,
    Int,
    Ptr,
    Fun(Vec<Ty>),
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
    pub symbols: BTreeMap<SymbolId, Symbol>,
    pub exp_symbols: BTreeMap<ExpId, SymbolId>,
    pub exp_vals: BTreeSet<ExpId>,
    pub exp_tys: BTreeMap<ExpId, Ty>,
    pub funs: BTreeMap<FunId, FunDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

#[derive(Clone, Copy, Debug)]
pub enum CmdArg {
    None,
    Int(i64),
    Ptr(usize),
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
    pub text: Vec<u8>,
    pub funs: BTreeMap<FunId, GenFunDef>,
    pub msgs: BTreeMap<MsgId, Msg>,
}

pub trait ShareSyntax {
    fn share_syntax(&self) -> Rc<Syntax>;
}

pub trait ExpVisitor: ShareSyntax {
    type Mode;
    type Output;

    fn on_err(&mut self, exp_id: ExpId, mode: Self::Mode, msg_id: MsgId) -> Self::Output;
    fn on_int(&mut self, exp_id: ExpId, mode: Self::Mode, value: i64) -> Self::Output;
    fn on_str(&mut self, exp_id: ExpId, mode: Self::Mode, value: &str) -> Self::Output;
    fn on_ident(&mut self, exp_id: ExpId, mode: Self::Mode, name: &str) -> Self::Output;
    fn on_call(
        &mut self,
        exp_id: ExpId,
        mode: Self::Mode,
        callee: ExpId,
        args: &[ExpId],
    ) -> Self::Output;
    fn on_index(
        &mut self,
        exp_id: ExpId,
        mode: Self::Mode,
        indexee: ExpId,
        arg: ExpId,
    ) -> Self::Output;
    fn on_bin(
        &mut self,
        exp_id: ExpId,
        mode: Self::Mode,
        op: Op,
        l: ExpId,
        r: ExpId,
    ) -> Self::Output;
    fn on_if(
        &mut self,
        exp_id: ExpId,
        mode: Self::Mode,
        cond: ExpId,
        body: ExpId,
        alt: ExpId,
    ) -> Self::Output;
    fn on_while(
        &mut self,
        exp_id: ExpId,
        mode: Self::Mode,
        cond: ExpId,
        body: ExpId,
    ) -> Self::Output;
    fn on_let(&mut self, exp_id: ExpId, mode: Self::Mode, pat: ExpId, init: ExpId) -> Self::Output;
    fn on_semi(&mut self, exp_id: ExpId, mode: Self::Mode, exps: &[ExpId]) -> Self::Output;

    fn on_exp(&mut self, exp_id: ExpId, mode: Self::Mode) -> Self::Output {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            &ExpKind::Err(msg_id) => self.on_err(exp_id, mode, msg_id),
            &ExpKind::Int(value) => self.on_int(exp_id, mode, value),
            ExpKind::Str(value) => self.on_str(exp_id, mode, value),
            ExpKind::Ident(name) => self.on_ident(exp_id, mode, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, mode, *callee, &args),
            ExpKind::Index { indexee, arg } => self.on_index(exp_id, mode, *indexee, *arg),
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, mode, op, l, r),
            &ExpKind::If { cond, body, alt } => self.on_if(exp_id, mode, cond, body, alt),
            &ExpKind::While { cond, body } => self.on_while(exp_id, mode, cond, body),
            &ExpKind::Let { pat, init } => self.on_let(exp_id, mode, pat, init),
            ExpKind::Semi(exps) => self.on_semi(exp_id, mode, &exps),
        }
    }
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
            Keyword::While => "while",
        }
    }

    fn get_all() -> &'static [Keyword] {
        &[
            Keyword::Let,
            Keyword::Def,
            Keyword::If,
            Keyword::Else,
            Keyword::While,
        ]
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

    fn is_successful(&self) -> bool {
        self.level != MsgLevel::Err
    }

    pub fn summarize<'a, I: Iterator<Item = &'a Msg>>(msgs: I, syntax: &Syntax) -> (bool, String) {
        use std::fmt::Write;

        let mut success = true;
        let mut stderr = String::new();
        for msg in msgs {
            let ((ly, lx), (ry, rx)) = syntax.locate_exp(msg.exp_id);
            let (ly, lx, ry, rx) = (ly + 1, lx + 1, ry + 1, rx + 1);
            writeln!(stderr, "At {}:{}..{}:{} {}", ly, lx, ry, rx, msg.message).unwrap();
            success = success && msg.level != MsgLevel::Err;
        }
        (success, stderr)
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

impl Prim {
    fn get_ty(self) -> Ty {
        match self {
            Prim::ByteToInt => Ty::Fun(vec![Ty::Byte, Ty::Int]),
            Prim::PrintLnInt => Ty::Fun(vec![Ty::Int, Ty::Unit]),
            Prim::ReadInt => Ty::Fun(vec![Ty::Int]),
        }
    }
}

impl Ty {
    fn make_str() -> Ty {
        Ty::Ptr
    }

    fn make_fun<T: Iterator<Item = Ty>>(args: T, result: Ty) -> Ty {
        Ty::Fun(args.chain(iter::once(result)).collect())
    }
}

impl Sema {
    fn is_successful(&self) -> bool {
        self.msgs.iter().all(|(_, msg)| msg.is_successful())
    }

    fn summarize_msgs(&self) -> (bool, String) {
        Msg::summarize(self.msgs.values(), &self.syntax)
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
    if !sema.is_successful() {
        let (success, stderr) = Msg::summarize(sema.msgs.values(), &sema.syntax);
        return CompilationResult {
            success,
            stderr,
            program: "".to_string(),
        };
    }

    gen_mir::gen_mir(sema)
}
