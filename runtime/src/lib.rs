use std::io::{self, BufRead, Write as IoWrite};
use std::str;

const REG_NUM: usize = 10;

macro_rules! define_op {
    ($($op:ident,)*) => {
        #[derive(Clone, Copy, PartialEq, Debug)]
        pub enum Op {
            $($op),*
        }

        pub fn deserialize_op(op: &str) -> Op {
            $(if op == stringify!($op) {
                return Op::$op;
            })*
            panic!("Unknown Op {}", op)
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

pub fn eval<R: io::Read, W: io::Write>(src: &str, stdin: R, stdout: W) {
    let mut ins = vec![];
    let mut strs = vec![];

    // Parse.
    for line in src.split("\n") {
        let line = line.trim_start();
        if line.starts_with("//") || line.len() == 0 {
            continue;
        }

        if line.starts_with(".text") {
            let value = line[".text ".len()..].to_owned();
            strs.push(value);
            continue;
        }

        let mut ops = line.split(" ");
        let op = ops.next().unwrap();
        let l = ops
            .next()
            .and_then(|x| x.parse::<usize>().ok())
            .unwrap_or(0);
        let r = ops.next().and_then(|x| x.parse::<i64>().ok()).unwrap_or(0);

        let op = deserialize_op(op);
        ins.push((op, l, r));
    }

    // Standard IO.
    let mut stdin = io::BufReader::new(stdin);
    let mut stdout = io::BufWriter::new(stdout);
    let mut stdin_words = Vec::new();
    let mut stdin_line = String::new();
    let mut next_word = move || {
        for _ in 0..10 {
            if let Some(word) = stdin_words.pop() {
                return word;
            }

            stdin_line.clear();
            stdin.read_line(&mut stdin_line).unwrap();
            stdin_words.extend(stdin_line.split_whitespace().map(String::from).rev());
        }
        panic!("Expected a word but not given.");
    };

    // Execution.
    let mut regs = [0; REG_NUM];
    let mut stack = Vec::new();
    stack.resize(1024, 0);
    let mut pc = 0;

    loop {
        let (op, l, r) = ins[pc];
        match op {
            Op::Imm => regs[l] = r,
            Op::AddImm => regs[l] += r,
            Op::Mov => regs[l] = regs[r as usize],
            Op::Jump => pc = r as usize,
            Op::Unless => {
                if regs[l] == 0 {
                    pc = r as usize
                }
            }
            Op::Push => {
                stack.push(regs[l]);
            }
            Op::Pop => regs[l] = stack.pop().unwrap(),
            Op::Load => regs[l] = stack[regs[r as usize] as usize],
            Op::Store => stack[regs[r as usize] as usize] = regs[l],
            Op::ToStr => {
                let t = regs[l].to_string();
                strs.push(t);
                regs[l] = (strs.len() - 1) as i64;
            }
            Op::StrCat => {
                let mut t = strs[regs[l] as usize].clone();
                t += &strs[regs[r as usize] as usize];
                strs.push(t);
                regs[l] = (strs.len() - 1) as i64;
            }
            Op::Add => regs[l] += regs[r as usize],
            Op::Sub => regs[l] -= regs[r as usize],
            Op::Mul => regs[l] *= regs[r as usize],
            Op::Div => regs[l] /= regs[r as usize],
            Op::Mod => regs[l] %= regs[r as usize],
            Op::Eq => regs[l] = bool_to_int(regs[l] == regs[r as usize]),
            Op::Ne => regs[l] = bool_to_int(regs[l] != regs[r as usize]),
            Op::Lt => regs[l] = bool_to_int(regs[l] < regs[r as usize]),
            Op::Le => regs[l] = bool_to_int(regs[l] <= regs[r as usize]),
            Op::Gt => regs[l] = bool_to_int(regs[l] > regs[r as usize]),
            Op::Ge => regs[l] = bool_to_int(regs[l] >= regs[r as usize]),
            Op::ReadInt => regs[l] = next_word().parse().unwrap_or(0),
            Op::ReadStr => {
                let word = next_word();
                strs.push(word);
                regs[l] = (strs.len() - 1) as i64;
            }
            Op::Print => write!(stdout, "{}", strs[regs[l] as usize]).unwrap(),
            Op::PrintLn => writeln!(stdout, "{}", strs[regs[l] as usize]).unwrap(),
            Op::Label | Op::Kill => {}
            Op::Call => unimplemented!(),
            Op::Ret => unimplemented!(),
            Op::Exit => return,
        }
        pc += 1;
    }
}

fn bool_to_int(x: bool) -> i64 {
    (if x { 1 } else { 0 })
}

pub fn eval_with_stdio(src: &str) {
    let stdin = io::stdin();
    let stdout = io::stdout();
    eval(src, stdin, stdout);
}
