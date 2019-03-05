use std::io::{self, BufRead, Write as IoWrite};
use std::mem::size_of;
use std::str;

const BASE_PTR_REG_ID: usize = 1;
const STACK_PTR_REG_ID: usize = 2;
const RET_REG_ID: usize = 3;
#[allow(unused)]
const KNOWN_REG_NUM: usize = 4;
const REG_NUM: usize = 12;

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
    Store8,
    Store,
    Load8,
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
    Alloc,
    Write,
    Exit,
}

pub fn eval<R: io::Read, W: io::Write>(src: &str, stdin: R, stdout: W) {
    let mut inss = vec![];
    let mut strs = vec![];

    // Parse.
    for line in src.split("\n") {
        let line = line.trim_left();
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
        inss.push((op, l, r));
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

    // Execute.
    let mem_size = 256 * 1024;
    let mut regs = [0_i64; REG_NUM];
    let mut mem = vec![0_u8; mem_size];
    let mut heap_size = 0;
    let mut frames = vec![];
    let mut pc = 0_usize;

    fn read<T: Copy>(mem: &[u8], p: usize) -> T {
        debug_assert!(p + size_of::<T>() <= mem.len());
        unsafe { *(mem.as_ptr().add(p) as *const T) }
    }

    fn write<T: Copy>(mem: &mut [u8], p: usize, value: T) {
        debug_assert!(p + size_of::<T>() <= mem.len());
        unsafe { *(mem.as_mut_ptr().add(p) as *mut T) = value }
    }

    regs[STACK_PTR_REG_ID] = mem.len() as i64;
    regs[BASE_PTR_REG_ID] = mem.len() as i64;

    loop {
        let (op, l, r) = inss[pc];
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
            Op::Call => {
                frames.push((pc, regs));
                pc = r as usize;
                regs[BASE_PTR_REG_ID] = regs[STACK_PTR_REG_ID];
            }
            Op::Ret => {
                let ret_val = regs[RET_REG_ID];
                let (ret_pc, ret_regs) = frames.pop().unwrap();
                pc = ret_pc;
                regs = ret_regs;
                regs[RET_REG_ID] = ret_val;
            }
            Op::Push => {
                regs[STACK_PTR_REG_ID] -= size_of::<i64>() as i64;
                let sp = regs[STACK_PTR_REG_ID] as usize;
                write::<i64>(&mut mem, sp, regs[l]);
            }
            Op::Pop => {
                let sp = regs[STACK_PTR_REG_ID] as usize;
                regs[l] = read::<i64>(&mem, sp);
                regs[STACK_PTR_REG_ID] += size_of::<i64>() as i64;
            }
            Op::Load8 => regs[l] = read::<u8>(&mem, regs[r as usize] as usize) as i64,
            Op::Load => regs[l] = read::<i64>(&mem, regs[r as usize] as usize),
            Op::Store8 => write::<u8>(&mut mem, regs[l] as usize, regs[r as usize] as u8),
            Op::Store => write::<i64>(&mut mem, regs[r as usize] as usize, regs[l]),
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
            Op::Alloc => {
                regs[l] = heap_size as i64;
                let size = regs[r as usize] as usize;
                heap_size += size;
            }
            Op::Write => {
                let p = regs[l] as usize;
                let size = regs[r as usize] as usize;
                stdout.write_all(&mem[p..p + size]).unwrap();
                stdout.flush().unwrap();
            }
            Op::Label | Op::Kill => {}
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
