use std::collections::BTreeMap;
use std::io::{self, Write as IoWrite};
use std::str;

const PUNS: &'static [&'static str] = &["(", ")"];
const BINS: &'static [&'static str] = &["++", "+", "-", "*", "/", "%"];

const EOF: &'static Tok = &Tok::Eof;

type TokId = usize;
type Range = (usize, usize);
type Toks = Vec<(Tok, Range)>;
type SynId = usize;
type RegId = usize;
type Off = usize;
type Ins = (Op, usize, usize);

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

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Op {
    Imm,
    Store,
    Load,
    Bin(&'static str),
    ToStr,
    ReadInt,
    ReadStr,
    Print,
    PrintLn,
    Exit,
}

struct Tokenizer {
    src: String,
    cur: usize,
    toks: Toks,
}

impl Tokenizer {
    fn c(&self) -> u8 {
        if self.cur >= self.src.as_bytes().len() {
            return 0;
        }
        self.src.as_bytes()[self.cur]
    }

    fn take<P: Fn(u8) -> bool>(&mut self, pred: P) -> Option<(String, Range)> {
        let l = self.cur;
        if !pred(self.c()) {
            return None;
        }
        while pred(self.c()) {
            self.cur += 1;
        }
        let r = self.cur;
        Some((self.src[l..r].into(), (l, r)))
    }

    fn expect(&mut self, prefix: &str) -> bool {
        if self.src[self.cur..].starts_with(prefix) {
            self.cur += prefix.len();
            return true;
        }
        false
    }

    fn tokenize(mut self) -> Vec<(Tok, Range)> {
        't: while self.cur < self.src.len() {
            let l = self.cur;
            if let Some(_) = self.take(is_whitespace) {
                continue;
            }
            if let Some((word, range)) = self.take(is_ascii_digit) {
                self.toks.push((Tok::Int(word.parse().unwrap_or(0)), range));
                continue;
            }
            if let Some((word, range)) = self.take(is_id_char) {
                self.toks.push((Tok::Id(word.into()), range));
                continue;
            }
            if self.c() == b'"' {
                self.cur += 1;
                let p = |c: u8| c != b'"' && c != b'\n' && c != 0;
                while p(self.c()) {
                    self.cur += 1;
                }
                let r = self.cur;
                self.cur += 1;
                let word = self.src[l + 1..r].into();
                self.toks.push((Tok::Str(word), (l, r + 1)));
                continue;
            }
            if self.expect("//") {
                self.take(|c| c != b'\n');
                continue;
            }
            for pun in PUNS {
                if self.expect(pun) {
                    self.toks.push((Tok::Pun(pun), (l, self.cur)));
                    continue 't;
                }
            }
            self.cur += 1;
            self.toks.push((Tok::Err("?".into()), (l, self.cur)));
        }
        self.toks.push((Tok::Eof, (self.cur, self.cur)));
        self.toks
    }
}

fn is_ascii_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_id_char(c: u8) -> bool {
    (b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z')
        || (is_ascii_digit(c) || b"!#$'*+-./<=>?@^_~".contains(&c))
}

fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\r' || c == b'\n'
}

struct Parser {
    toks: Toks,
    cur: usize,
    syns: Vec<Syn>,
}

impl Parser {
    fn next(&self) -> &Tok {
        if self.cur >= self.toks.len() {
            return EOF;
        }
        &self.toks[self.cur].0
    }

    fn next_is_opening(&self) -> bool {
        match self.next() {
            &Tok::Pun("(") => true,
            _ => false,
        }
    }

    fn next_is_closing(&self) -> bool {
        match self.next() {
            &Tok::Pun(")") => true,
            _ => false,
        }
    }

    fn push(&mut self, syn: Syn) -> SynId {
        let syn_id = self.syns.len();
        self.syns.push(syn);
        syn_id
    }

    fn read_exp(&mut self) -> SynId {
        if self.next_is_opening() {
            self.cur += 1;
            let mut children = vec![];
            while !self.next_is_closing() && *self.next() != Tok::Eof {
                children.push(self.read_exp());
            }
            if *self.next() != Tok::Eof {
                self.cur += 1;
            }
            return self.push(Syn::App(children));
        }
        if self.next_is_closing() {
            self.cur += 1;
            let tok_id = self.cur;
            return self.push(Syn::Err("Unmatched bracket".into(), tok_id));
        }

        let tok_id = self.cur;
        self.cur += 1;
        self.push(Syn::Val(tok_id))
    }

    fn parse(mut self) -> Vec<Syn> {
        self.read_exp();
        self.syns
    }
}

pub struct Compiler {
    toks: Toks,
    syns: Vec<Syn>,
    ins: Vec<Ins>,
    reg_num: usize,
    env: BTreeMap<String, usize>,
    off: Off,
    strs: Vec<String>,
}

impl Compiler {
    fn new_reg(&mut self) -> usize {
        self.reg_num += 1;
        self.reg_num - 1
    }

    fn push(&mut self, op: Op, l: RegId, r: usize) -> RegId {
        self.ins.push((op, l, r));
        l
    }

    fn on_tok(&mut self, tok_id: usize) -> RegId {
        match &self.toks[tok_id].0 {
            &Tok::Err(ref err) => panic!("{}", err),
            &Tok::Id(ref name) => {
                if let Some(&off) = self.env.get(name) {
                    let l = self.new_reg();
                    self.push(Op::Load, l, off)
                } else {
                    panic!("var undefined {}", name)
                }
            }
            &Tok::Int(value) => {
                let l = self.new_reg();
                self.push(Op::Imm, l, value as usize)
            }
            &Tok::Str(_) => {
                let value = match self.toks[tok_id].0.clone() {
                    Tok::Str(value) => value,
                    _ => unreachable!(),
                };
                let l = self.new_reg();
                self.strs.push(value.to_owned());
                self.push(Op::Imm, l, self.strs.len() - 1)
            }
            &Tok::Pun(_) | Tok::Eof => unreachable!(),
        }
    }

    fn to_str(&self, syn_id: SynId) -> &str {
        if let &Syn::Val(tok_id) = &self.syns[syn_id] {
            match &self.toks[tok_id].0 {
                &Tok::Id(ref id) => id,
                &Tok::Str(ref str) => str,
                tok => panic!("{:?} must be str or id", tok),
            }
        } else {
            panic!("{} must be an str or id", syn_id)
        }
    }

    fn do_pri(&mut self, name: &str, syns: &[SynId]) -> RegId {
        if let Some(bin_id) = BINS.iter().position(|&x| x == name) {
            let l = self.on_exp(syns[0]);
            for i in 1..syns.len() {
                let r = self.on_exp(syns[i]);
                self.push(Op::Bin(BINS[bin_id]), l, r);
            }
            l
        } else if name == "let" {
            let name = self.to_str(syns[0]).into();
            let l = self.on_exp(syns[1]);
            let off = self.off;
            self.off += 1;
            self.env.insert(name, off);
            self.push(Op::Store, l, off)
        } else if name == "to_str" {
            let l = self.on_exp(syns[0]);
            self.push(Op::ToStr, l, 0)
        } else if name == "read_int" {
            let l = self.new_reg();
            self.push(Op::ReadInt, l, 0)
        } else if name == "read_str" {
            let l = self.new_reg();
            self.push(Op::ReadStr, l, 0)
        } else if name == "print" {
            for i in 0..syns.len() {
                let l = self.on_exp(syns[i]);
                self.push(Op::Print, l, 0);
            }
            0
        } else if name == "println" {
            for i in 0..syns.len() {
                let last = i + 1 == syns.len();
                let l = self.on_exp(syns[i]);
                self.push(if last { Op::PrintLn } else { Op::Print }, l, 0);
            }
            0
        } else {
            unimplemented!("{}", name)
        }
    }

    fn on_exp(&mut self, syn_id: SynId) -> RegId {
        let syn = self.syns[syn_id].clone();
        match syn {
            Syn::Err(err, _) => panic!("{}", err),
            Syn::Val(tok_id) => self.on_tok(tok_id),
            Syn::App(syns) => {
                if let Syn::Val(tok_id) = self.syns[syns[0]] {
                    match self.toks[tok_id].0.clone() {
                        Tok::Id(head) => self.do_pri(&head, &syns[1..]),
                        _ => panic!("{:?} callee must be identifier", &self.toks[syns[0]]),
                    }
                } else {
                    panic!("{}")
                }
            }
        }
    }

    fn compile(mut self) -> (Vec<Ins>, usize, usize, Vec<String>) {
        writeln!(io::stderr(), "toks={:?}\nsyns={:?}", self.toks, self.syns).unwrap();

        self.on_exp(self.syns.len() - 1);
        self.push(Op::Exit, 0, 0);

        writeln!(io::stderr(), "ins={:?}", self.ins).unwrap();

        (self.ins, self.reg_num, self.off, self.strs)
    }
}

pub struct Evaluator<R, W> {
    ins: Vec<Ins>,
    reg_num: usize,
    off: usize,
    strs: Vec<String>,
    stdin_line: String,
    stdin_words: Vec<String>,
    stdin: R,
    stdout: W,
}

impl<R: io::BufRead, W: IoWrite> Evaluator<R, W> {
    fn next_word(&mut self) -> String {
        for _ in 0..10 {
            if let Some(word) = self.stdin_words.pop() {
                return word;
            }

            self.stdin_line.clear();
            self.stdin.read_line(&mut self.stdin_line).unwrap();
            self.stdin_words
                .extend(self.stdin_line.split_whitespace().map(String::from).rev());
        }
        panic!("Expected a word but not given.");
    }

    fn eval(mut self) {
        let mut regs = vec![0; self.reg_num];
        let mut stack = vec![0; self.off];
        let mut strs = self.strs.to_owned();
        let mut pc = 0;
        loop {
            let (op, l, r) = self.ins[pc];
            pc += 1;
            match op {
                Op::Imm => regs[l] = r as i64,
                Op::Load => regs[l] = stack[r],
                Op::Store => stack[r] = regs[l],
                Op::ToStr => {
                    let t = regs[l].to_string();
                    strs.push(t);
                    regs[l] = (strs.len() - 1) as i64;
                }
                Op::Bin("++") => {
                    let mut t = strs[regs[l] as usize].clone();
                    t += &strs[regs[r] as usize];
                    strs.push(t);
                    regs[l] = (strs.len() - 1) as i64;
                }
                Op::Bin("+") => regs[l] += regs[r],
                Op::Bin("-") => regs[l] -= regs[r],
                Op::Bin("*") => regs[l] *= regs[r],
                Op::Bin("/") => regs[l] /= regs[r],
                Op::Bin("%") => regs[l] %= regs[r],
                Op::Bin(_) => unimplemented!(),
                Op::ReadInt => regs[l] = self.next_word().parse().unwrap_or(0),
                Op::ReadStr => {
                    strs.push(self.next_word());
                    regs[l] = (strs.len() - 1) as i64;
                }
                Op::Print => write!(self.stdout, "{}", strs[regs[l] as usize]).unwrap(),
                Op::PrintLn => writeln!(self.stdout, "{}", strs[regs[l] as usize]).unwrap(),
                Op::Exit => return,
            }
        }
    }
}

pub fn compile(src: &str) -> (Vec<Ins>, usize, usize, Vec<String>) {
    let src = src.to_owned();
    let toks = Tokenizer {
        src: src.clone(),
        toks: vec![],
        cur: 0,
    }
    .tokenize();
    let syns = Parser {
        toks: toks.clone(),
        cur: 0,
        syns: vec![],
    }
    .parse();
    Compiler {
        toks: toks,
        syns: syns,
        ins: vec![],
        reg_num: 0,
        env: BTreeMap::new(),
        off: 0,
        strs: vec![],
    }
    .compile()
}

pub fn eval(src: &str, stdin: &str) -> String {
    let mut stdout = Vec::new();
    let (ins, reg_num, off, strs) = compile(src);
    Evaluator {
        ins: ins,
        reg_num: reg_num,
        off: off,
        strs: strs,
        stdin_line: String::new(),
        stdin_words: Vec::new(),
        stdin: io::BufReader::new(io::Cursor::new(&stdin)),
        stdout: io::BufWriter::new(&mut stdout),
    }
    .eval();
    String::from_utf8(stdout).unwrap()
}

pub fn eval_with_stdio(src: &str) {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let (ins, reg_num, off, strs) = compile(src);
    Evaluator {
        ins: ins,
        reg_num: reg_num,
        off: off,
        strs: strs,
        stdin_line: String::new(),
        stdin_words: Vec::new(),
        stdin: io::BufReader::new(stdin.lock()),
        stdout: io::BufWriter::new(stdout.lock()),
    }
    .eval();
}
