//! バーチャルマシン (virtual machine)

use crate::*;

pub(crate) struct VM {
    asm: AlAsm,
    table_tys: Vec<CellTy>,
    table: Vec<i64>,
    heap_end: usize,
    stack_end: usize,
    pc: usize,
}

impl VM {
    pub(crate) fn new(asm: AlAsm) -> VM {
        VM {
            asm,
            table_tys: vec![],
            table: vec![],
            heap_end: 0,
            stack_end: 0,
            pc: 0,
        }
    }

    fn instrs(&self) -> &[InstrKind] {
        &self.asm.instrs
    }

    fn table_initialize(&mut self) {
        unsafe fn resize_vec<T>(v: &mut Vec<T>, len: usize) {
            v.reserve(len);
            v.set_len(len);
            std::ptr::write_bytes(v.as_mut_ptr(), 0xcd, len);
        };

        let len = 1024;

        unsafe { resize_vec(&mut self.table_tys, len) };
        unsafe { resize_vec(&mut self.table, len) };
        self.stack_end = len;
    }

    pub(crate) fn table_get(&self, addr: usize) -> (CellTy, i64) {
        (self.table_tys[addr], self.table[addr])
    }

    pub(crate) fn table_set(&mut self, addr: usize, pair: (CellTy, i64)) {
        self.table_tys[addr] = pair.0;
        self.table[addr] = pair.1;
    }

    pub(crate) fn heap_alloc(&mut self, count: usize) -> (usize, usize) {
        let start = self.heap_end;
        self.heap_end += count;
        let end = self.heap_end;
        (start, end)
    }

    pub(crate) fn stack_push(&mut self, (ty, value): (CellTy, i64)) {
        self.stack_end -= 1;
        self.table_set(self.stack_end, (ty, value));
    }

    pub(crate) fn stack_pop(&mut self) -> (CellTy, i64) {
        self.stack_end += 1;
        self.table_get(self.stack_end - 1)
    }

    pub(crate) fn stack_pop_val(&mut self) -> (CellTy, i64) {
        match self.stack_pop() {
            (CellTy::Cell, addr) => self.table_get(addr as usize),
            (ty, value) => (ty, value),
        }
    }

    fn initialize(&mut self) {
        self.table_initialize();
        self.heap_alloc(self.asm.global_count());
    }

    pub(crate) fn run(&mut self) -> ! {
        self.initialize();

        loop {
            self.pc += 1;
            match self.instrs()[self.pc - 1] {
                InstrKind::Exit => std::process::exit(0),
                InstrKind::Assert => {
                    if self.stack_pop() != (CellTy::Bool, 1) {
                        eprintln!("assertion error");
                        std::process::abort()
                    }
                }
                InstrKind::CellSet => {
                    let right = self.stack_pop();
                    let left = match self.stack_pop() {
                        (CellTy::Cell, id) => id as usize,
                        _ => panic!("Type error"),
                    };
                    self.table_set(left, right);
                }
                InstrKind::Bool(value) => {
                    self.stack_push((CellTy::Bool, value as i64));
                }
                InstrKind::Int(value) => {
                    self.stack_push((CellTy::Int, value));
                }
                InstrKind::GlobalGet => match self.stack_pop() {
                    (CellTy::Int, addr) => {
                        self.stack_push((CellTy::Cell, addr));
                    }
                    _ => panic!("Type error"),
                },
                InstrKind::OpAdd => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => {
                            self.stack_push((CellTy::Int, left + right))
                        }
                        _ => panic!("Type error"),
                    }
                }
                InstrKind::OpSub => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => {
                            self.stack_push((CellTy::Int, left - right))
                        }
                        _ => panic!("Type error"),
                    }
                }
                InstrKind::OpMul => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => {
                            self.stack_push((CellTy::Int, left * right))
                        }
                        _ => panic!("Type error"),
                    }
                }
                InstrKind::OpDiv => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    // FIXME: ゼロ除算をエラーにする
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => {
                            self.stack_push((CellTy::Int, left / right))
                        }
                        _ => panic!("Type error"),
                    }
                }
                InstrKind::OpEq => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Bool, left), (CellTy::Bool, right)) => {
                            self.stack_push((CellTy::Bool, (left == right) as i64))
                        }
                        ((CellTy::Int, left), (CellTy::Int, right)) => {
                            self.stack_push((CellTy::Bool, (left == right) as i64))
                        }
                        _ => self.stack_push((CellTy::Bool, false as i64)),
                    }
                }
            }
        }
    }
}
