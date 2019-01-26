use std::collections::BTreeMap;
use std::io::{self, Write as IoWrite};
use std::str;

const PUNS: &'static [&'static str] = &["(", ")"];
const BINS: &'static [&'static str] = &["++", "+", "-", "*", "/", "%"];
const RELS: &'static [&'static str] = &["==", "!=", "<", "<=", ">", ">="];

const EOF: &'static Tok = &Tok::Eof;

type TokId = usize;
type Range = (usize, usize);
type Toks = Vec<(Tok, Range)>;
type SynId = usize;
type RegId = usize;
type LabId = usize;
type VarId = usize;
type EnvId = usize;
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
	Mov,
	Store,
	Load,
	Label,
	Jump,
	Unless,
	Bin(&'static str),
	ToStr,
	ReadInt,
	ReadStr,
	Print,
	PrintLn,
	Exit,
}

#[derive(Default)]
struct Env {
	parent_env_id: usize,
	vars: BTreeMap<String, VarId>,
}

#[derive(Clone, Default)]
pub struct Program {
	ins: Vec<Ins>,
	strs: Vec<String>,
	reg_num: usize,
	var_num: usize,
	lab_num: usize,
}

#[derive(Default)]
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
			if self.expect("//") {
				self.take(|c| c != b'\n');
				continue;
			}
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

fn is_ascii_digit(c: u8) -> bool { b'0' <= c && c <= b'9' }

fn is_id_char(c: u8) -> bool {
	(b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z' || is_ascii_digit(c))
		|| b"!#$'*+-./%<=>?@^_~".contains(&c)
}

fn is_whitespace(c: u8) -> bool { c == b' ' || c == b'\t' || c == b'\r' || c == b'\n' }

#[derive(Default)]
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

#[derive(Default)]
pub struct Compiler {
	toks: Toks,
	syns: Vec<Syn>,
	p: Program,
	envs: Vec<Env>,
	env_id: EnvId,
}

impl Compiler {
	fn new_reg(&mut self) -> RegId {
		self.p.reg_num += 1;
		self.p.reg_num - 1
	}

	fn new_var(&mut self) -> VarId {
		self.p.var_num += 1;
		self.p.var_num - 1
	}

	fn new_lab(&mut self) -> LabId {
		self.p.lab_num += 1;
		self.p.lab_num - 1
	}

	fn push(&mut self, op: Op, l: RegId, r: usize) -> RegId {
		self.p.ins.push((op, l, r));
		l
	}

	fn push_str(&mut self, value: String) -> usize {
		self.p.strs.push(value.to_owned());
		self.p.strs.len() - 1
	}

	fn replace_env(&mut self, parent_env_id: EnvId) {
		let mut env = Env::default();
		env.parent_env_id = parent_env_id;
		self.envs.push(env);
		self.env_id = self.envs.len() - 1;
	}

	fn find_var_id(&self, name: &str) -> Option<VarId> {
		let mut env_id = self.env_id;
		loop {
			if let Some(&var_id) = self.envs[env_id].vars.get(name) {
				return Some(var_id);
			}
			if env_id == 0 {
				return None;
			}
			env_id = self.envs[env_id].parent_env_id;
		}
	}

	fn on_tok(&mut self, tok_id: TokId) -> RegId {
		match self.toks[tok_id].0.clone() {
			Tok::Err(err) => panic!("{}", err),
			Tok::Id(name) => {
				if let Some(var_id) = self.find_var_id(&name) {
					let l = self.new_reg();
					self.push(Op::Load, l, var_id)
				} else if name == "true" {
					let l = self.new_reg();
					self.push(Op::Imm, l, 1)
				} else if name == "false" {
					let l = self.new_reg();
					self.push(Op::Imm, l, 0)
				} else {
					panic!("var undefined {}", name)
				}
			}
			Tok::Int(value) => {
				let l = self.new_reg();
				self.push(Op::Imm, l, value as usize)
			}
			Tok::Str(_) => {
				let value = match self.toks[tok_id].0.clone() {
					Tok::Str(value) => value,
					_ => unreachable!(),
				};
				let str_id = self.push_str(value);
				let l = self.new_reg();
				self.push(Op::Imm, l, str_id)
			}
			Tok::Pun(_) | Tok::Eof => unreachable!(),
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
		} else if let Some(id) = RELS.iter().position(|&x| x == name) {
			let l = self.on_exp(syns[0]);
			let r = self.on_exp(syns[1]);
			self.push(Op::Bin(RELS[id]), l, r)
		} else if name == "let" {
			let name = self.to_str(syns[0]).into();
			// Create local variable. Shadow if existing.
			let var_id = self.new_var();
			self.envs[self.env_id].vars.insert(name, var_id);
			// Set default value.
			let l = self.on_exp(syns[1]);
			self.push(Op::Store, l, var_id)
		} else if name == "begin" {
			let cur_env_id = self.env_id;
			self.replace_env(cur_env_id);
			let mut l = 0;
			for i in 0..syns.len() {
				l = self.on_exp(syns[i]);
			}
			self.env_id = cur_env_id;
			l
		} else if name == "cond" {
			// Label to point to the end of cond.
			let end_lab = self.new_lab();
			// Register to set the result of cond.
			let end_reg = self.new_reg();
			let mut i = 0;
			while i < syns.len() {
				if i + 1 < syns.len() {
					// .. cond then_cl ..
					let else_lab = self.new_lab();
					let cond = self.on_exp(syns[i]);
					// Unless cond is true, go to next clause.
					self.push(Op::Unless, cond, else_lab);
					// Evaluate the then clause.
					let then_cl = self.on_exp(syns[i + 1]);
					// Set the result to the result register.
					self.push(Op::Mov, end_reg, then_cl);
					// Jump to the end of cond.
					self.push(Op::Jump, 0, end_lab);
					// Come from the `unless`.
					self.push(Op::Label, 0, else_lab);
					i += 2;
				} else {
					// .. else_cl
					let else_cl = self.on_exp(syns[i]);
					self.push(Op::Mov, end_reg, else_cl);
					i += 1;
				}
			}
			self.push(Op::Label, 0, end_lab);
			end_reg
		} else if name == "while" {
			let beg_lab = self.new_lab();
			let end_lab = self.new_lab();
			// Register to set the result of cond.

			self.push(Op::Label, 0, beg_lab);
			let cond = self.on_exp(syns[0]);
			// Unless cond is true, goto end clause.
			self.push(Op::Unless, cond, end_lab);
			// Evaluate the body.
			self.on_exp(syns[1]);
			// Jump to the begin label to continue.
			self.push(Op::Jump, 0, beg_lab);
			// Come on break.
			self.push(Op::Label, 0, end_lab);
			// No result.
			0
		} else if name == "set" {
			let name = self.to_str(syns[0]).to_owned();
			let l = self.on_exp(syns[1]);
			let var_id = self.find_var_id(&name).expect("unknown var");
			self.push(Op::Store, l, var_id)
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
			panic!("unknown callee {}", name)
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
						tok => panic!("{:?} callee must be identifier", &tok),
					}
				} else {
					panic!("callee must be identifier")
				}
			}
		}
	}

	fn compile(mut self) -> Program {
		writeln!(io::stderr(), "toks={:?}\nsyns={:?}", self.toks, self.syns).unwrap();

		self.replace_env(0); // Global env.

		let entry_syn_id = self.syns.len() - 1;
		self.on_exp(entry_syn_id);
		self.push(Op::Exit, 0, 0);

		writeln!(io::stderr(), "ins={:?}", self.p.ins).unwrap();

		self.p
	}
}

pub struct Evaluator<R, W> {
	p: Program,
	labels: Vec<usize>,
	regs: Vec<i64>,
	stack: Vec<i64>,
	strs: Vec<String>,
	pc: usize,
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
			self.stdin_words.extend(self.stdin_line.split_whitespace().map(String::from).rev());
		}
		panic!("Expected a word but not given.");
	}

	fn eval_ins(&mut self, op: Op, l: usize, r: usize) {
		match op {
			Op::Imm => self.regs[l] = r as i64,
			Op::Mov => self.regs[l] = self.regs[r],
			Op::Jump => self.pc = self.labels[r],
			Op::Unless => {
				if self.regs[l] == 0 {
					self.pc = self.labels[r]
				}
			}
			Op::Load => self.regs[l] = self.stack[r],
			Op::Store => self.stack[r] = self.regs[l],
			Op::ToStr => {
				let t = self.regs[l].to_string();
				self.strs.push(t);
				self.regs[l] = (self.strs.len() - 1) as i64;
			}
			Op::Bin("++") => {
				let mut t = self.strs[self.regs[l] as usize].clone();
				t += &self.strs[self.regs[r] as usize];
				self.strs.push(t);
				self.regs[l] = (self.strs.len() - 1) as i64;
			}
			Op::Bin("+") => self.regs[l] += self.regs[r],
			Op::Bin("-") => self.regs[l] -= self.regs[r],
			Op::Bin("*") => self.regs[l] *= self.regs[r],
			Op::Bin("/") => self.regs[l] /= self.regs[r],
			Op::Bin("%") => self.regs[l] %= self.regs[r],
			Op::Bin("==") => self.regs[l] = bool_to_int(self.regs[l] == self.regs[r]),
			Op::Bin("!=") => self.regs[l] = bool_to_int(self.regs[l] != self.regs[r]),
			Op::Bin("<") => self.regs[l] = bool_to_int(self.regs[l] < self.regs[r]),
			Op::Bin("<=") => self.regs[l] = bool_to_int(self.regs[l] <= self.regs[r]),
			Op::Bin(">") => self.regs[l] = bool_to_int(self.regs[l] > self.regs[r]),
			Op::Bin(">=") => self.regs[l] = bool_to_int(self.regs[l] >= self.regs[r]),
			Op::Bin(_) => unimplemented!(),
			Op::ReadInt => self.regs[l] = self.next_word().parse().unwrap_or(0),
			Op::ReadStr => {
				let word = self.next_word();
				self.strs.push(word);
				self.regs[l] = (self.strs.len() - 1) as i64;
			}
			Op::Print => write!(self.stdout, "{}", self.strs[self.regs[l] as usize]).unwrap(),
			Op::PrintLn => writeln!(self.stdout, "{}", self.strs[self.regs[l] as usize]).unwrap(),
			Op::Label => {}
			Op::Exit => return,
		}
	}

	fn eval(mut self) {
		// Build jump table.
		for pc in 0..self.p.ins.len() {
			if let (Op::Label, _, r) = self.p.ins[pc] {
				self.labels[r] = pc + 1;
			}
		}

		loop {
			let (op, l, r) = self.p.ins[self.pc];
			if op == Op::Exit {
				break;
			}
			self.pc += 1;
			self.eval_ins(op, l, r);
		}
	}
}

fn bool_to_int(x: bool) -> i64 { (if x { 1 } else { 0 }) }

pub fn compile(src: &str) -> Program {
	let src = src.to_owned();
	let toks = Tokenizer { src: src.clone(), toks: vec![], cur: 0 }.tokenize();
	let syns = Parser { toks: toks.clone(), cur: 0, syns: vec![] }.parse();
	Compiler { toks: toks, syns: syns, p: Program::default(), envs: Vec::new(), env_id: 0 }.compile()
}

pub fn exec<R: io::Read, W: io::Write>(program: Program, stdin: R, stdout: W) {
	Evaluator {
		labels: vec![0; program.lab_num],
		regs: vec![0; program.reg_num],
		stack: vec![0; program.var_num], // Allocate static vars.
		strs: program.strs.to_owned(),   // Allocate string literals.
		pc: 0,
		p: program,
		stdin_line: String::new(),
		stdin_words: Vec::new(),
		stdin: io::BufReader::new(stdin),
		stdout: io::BufWriter::new(stdout),
	}
	.eval();
}

pub fn eval_with_stdio(src: &str) {
	let program = compile(src);
	let stdin = io::stdin();
	let stdout = io::stdout();
	exec(program, stdin, stdout);
}
