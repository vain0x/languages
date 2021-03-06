use std::io::{self, BufRead, Write as IoWrite};
use std::mem::size_of;
use std::str;

#[allow(unused)]
use std::io::stderr;

const BASE_PTR_REG_ID: usize = 1;
const STACK_PTR_REG_ID: usize = 2;
#[allow(unused)]
const RET_REG_ID: usize = 3;
#[allow(unused)]
const KNOWN_REG_NUM: usize = 4;
const REG_NUM: usize = 12;

macro_rules! define_cmd {
    ($($name:ident,)*) => {
        #[derive(Clone, Copy, PartialEq, Debug)]
        pub enum Cmd {
            $($name),*
        }

        pub fn deserialize_cmd(cmd: &str) -> Cmd {
            $(if cmd == stringify!($name) {
                return Cmd::$name;
            })*
            panic!("Unknown Cmd {}", cmd)
        }
    };
}

define_cmd! {
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
    PushRegs,
    PopRegs,
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
    BitAnd,
    BitOr,
    BitXor,
    BitShiftL,
    BitShiftR,
    ReadInt,
    ReadStr,
    PrintLnInt,
    Alloc,
    Write,
    Exit,
}

pub fn eval<R: io::Read, W: io::Write>(src: &str, stdin: R, stdout: W) {
    let mut inss = vec![];
    let mut text = vec![];

    // Parse.
    for line in src.split("\n") {
        #[allow(deprecated)]
        let line = line.trim_left();
        if line.starts_with("//") || line.len() == 0 {
            continue;
        }

        if line.starts_with(".text") {
            text = line[".text ".len()..]
                .split(",")
                .map(|b| b.parse().ok())
                .collect::<Option<Vec<u8>>>()
                .unwrap_or(vec![]);
            continue;
        }

        let mut cmds = line.split(" ");
        let cmd = cmds.next().unwrap();
        let l = cmds
            .next()
            .and_then(|x| x.parse::<usize>().ok())
            .unwrap_or(0);
        let r = cmds.next().and_then(|x| x.parse::<i64>().ok()).unwrap_or(0);

        let cmd = deserialize_cmd(cmd);
        inss.push((cmd, l, r));
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
    let mut pc = 0_usize;

    fn read<T: Copy>(mem: &[u8], p: usize) -> T {
        debug_assert!(p + size_of::<T>() <= mem.len());
        unsafe { *(mem.as_ptr().offset(p as isize) as *const T) }
    }

    fn write<T: Copy>(mem: &mut [u8], p: usize, value: T) {
        debug_assert!(p + size_of::<T>() <= mem.len());
        unsafe { *(mem.as_mut_ptr().offset(p as isize) as *mut T) = value }
    }

    regs[STACK_PTR_REG_ID] = mem.len() as i64;
    regs[BASE_PTR_REG_ID] = mem.len() as i64;

    heap_size += text.len();
    for i in 0..text.len() {
        mem[i] = text[i];
    }

    loop {
        let (cmd, l, r) = inss[pc];
        match cmd {
            Cmd::Imm => regs[l] = r,
            Cmd::AddImm => regs[l] += r,
            Cmd::Mov => regs[l] = regs[r as usize],
            Cmd::Jump => pc = r as usize,
            Cmd::Unless => {
                if regs[l] == 0 {
                    pc = r as usize
                }
            }
            Cmd::Call => {
                regs[STACK_PTR_REG_ID] -= size_of::<i64>() as i64;
                let sp = regs[STACK_PTR_REG_ID] as usize;
                write::<usize>(&mut mem, sp, pc);
                pc = r as usize;
            }
            Cmd::Ret => {
                let sp = regs[STACK_PTR_REG_ID] as usize;
                let r = read::<usize>(&mem, sp);
                regs[STACK_PTR_REG_ID] += size_of::<i64>() as i64;
                pc = r;
            }
            Cmd::Push => {
                regs[STACK_PTR_REG_ID] -= size_of::<i64>() as i64;
                let sp = regs[STACK_PTR_REG_ID] as usize;
                write::<i64>(&mut mem, sp, regs[l]);
            }
            Cmd::Pop => {
                let sp = regs[STACK_PTR_REG_ID] as usize;
                regs[l] = read::<i64>(&mem, sp);
                regs[STACK_PTR_REG_ID] += size_of::<i64>() as i64;
            }
            Cmd::PushRegs => {
                let mut sp = regs[STACK_PTR_REG_ID] as usize;
                for i in KNOWN_REG_NUM..REG_NUM {
                    sp -= size_of::<i64>();
                    write::<i64>(&mut mem, sp, regs[i]);
                }
                regs[STACK_PTR_REG_ID] = sp as i64;
            }
            Cmd::PopRegs => {
                let mut sp = regs[STACK_PTR_REG_ID] as usize;
                for i in (KNOWN_REG_NUM..REG_NUM).rev() {
                    regs[i] = read::<i64>(&mut mem, sp);
                    sp += size_of::<i64>();
                }
                regs[STACK_PTR_REG_ID] = sp as i64;
            }
            Cmd::Load8 => regs[l] = read::<u8>(&mem, regs[r as usize] as usize) as i64,
            Cmd::Load => regs[l] = read::<i64>(&mem, regs[r as usize] as usize),
            Cmd::Store8 => write::<u8>(&mut mem, regs[l] as usize, regs[r as usize] as u8),
            Cmd::Store => write::<i64>(&mut mem, regs[l] as usize, regs[r as usize]),
            Cmd::Add => regs[l] += regs[r as usize],
            Cmd::Sub => regs[l] -= regs[r as usize],
            Cmd::Mul => regs[l] *= regs[r as usize],
            Cmd::Div => regs[l] /= regs[r as usize],
            Cmd::Mod => regs[l] %= regs[r as usize],
            Cmd::Eq => regs[l] = bool_to_int(regs[l] == regs[r as usize]),
            Cmd::Ne => regs[l] = bool_to_int(regs[l] != regs[r as usize]),
            Cmd::Lt => regs[l] = bool_to_int(regs[l] < regs[r as usize]),
            Cmd::Le => regs[l] = bool_to_int(regs[l] <= regs[r as usize]),
            Cmd::Gt => regs[l] = bool_to_int(regs[l] > regs[r as usize]),
            Cmd::Ge => regs[l] = bool_to_int(regs[l] >= regs[r as usize]),
            Cmd::BitAnd => regs[l] = regs[l] & regs[r as usize],
            Cmd::BitOr => regs[l] = regs[l] | regs[r as usize],
            Cmd::BitXor => regs[l] = regs[l] ^ regs[r as usize],
            Cmd::BitShiftL => regs[l] = ((regs[l] as usize) << regs[r as usize]) as i64,
            Cmd::BitShiftR => regs[l] = (regs[l] as usize >> regs[r as usize]) as i64,
            Cmd::ReadInt => regs[l] = next_word().parse().unwrap_or(0),
            Cmd::ReadStr => {
                let word = next_word();
                let p = heap_size;
                for i in 0..word.as_bytes().len() {
                    mem[p + i] = word.as_bytes()[i];
                }
                heap_size += word.len();
                regs[l] = make_ptr(p, heap_size);
            }
            Cmd::PrintLnInt => writeln!(stdout, "{}", regs[r as usize]).unwrap(),
            Cmd::Alloc => {
                let p = heap_size;
                heap_size += regs[r as usize] as usize;
                let q = heap_size;
                regs[l] = make_ptr(p, q);
            }
            Cmd::Write => {
                let (p, q) = decompose_ptr(regs[l]);
                stdout.write_all(&mem[p..q]).unwrap();
                stdout.flush().unwrap();
            }
            Cmd::Label | Cmd::Kill => {}
            Cmd::Exit => return,
        }
        pc += 1;
    }
}

fn bool_to_int(x: bool) -> i64 {
    (if x { 1 } else { 0 })
}

fn make_ptr(p: usize, q: usize) -> i64 {
    (q << 32 | p) as i64
}

fn decompose_ptr(x: i64) -> (usize, usize) {
    ((x & 0xFFFF_FFFF) as usize, x as usize >> 32)
}

pub fn eval_with_stdio(src: &str) {
    let stdin = io::stdin();
    let stdout = io::stdout();
    eval(src, stdin, stdout);
}
