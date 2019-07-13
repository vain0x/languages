use crate::ins::*;

pub(crate) mod il;
pub(crate) mod ins;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CellTy {
    None,
    Bool,
    Int,
    Cell,
}

pub(crate) struct Runtime {
    table_tys: Vec<CellTy>,
    table: Vec<i64>,
    heap_end: usize,
    stack_end: usize,
    inss: Vec<InsKind>,
    pc: usize,
}

impl Runtime {
    fn new() -> Self {
        Runtime {
            table_tys: vec![CellTy::None; 1024],
            table: vec![0xcd; 1024],
            heap_end: 0,
            stack_end: 1024,
            inss: vec![],
            pc: 0,
        }
    }

    fn table_get(&self, addr: usize) -> (CellTy, i64) {
        (self.table_tys[addr], self.table[addr])
    }

    fn table_set(&mut self, addr: usize, pair: (CellTy, i64)) {
        self.table_tys[addr] = pair.0;
        self.table[addr] = pair.1;
    }

    fn heap_alloc(&mut self, count: usize) -> (usize, usize) {
        let start = self.heap_end;
        self.heap_end += count;
        let end = self.heap_end;
        (start, end)
    }

    fn stack_push(&mut self, (ty, value): (CellTy, i64)) {
        self.stack_end -= 1;
        self.table_set(self.stack_end, (ty, value));
    }

    fn stack_pop(&mut self) -> (CellTy, i64) {
        self.stack_end += 1;
        self.table_get(self.stack_end - 1)
    }

    fn stack_pop_val(&mut self) -> (CellTy, i64) {
        match self.stack_pop() {
            (CellTy::Cell, addr) => {
                self.table_get(addr as usize)
            }
            (ty, value) => (ty, value)
        }
    }

    fn run(&mut self) -> ! {
        loop {
            self.pc += 1;
            match self.inss[self.pc - 1] {
                InsKind::Exit => std::process::exit(0),
                InsKind::Assert => {
                    if self.stack_pop() != (CellTy::Bool, 1) {
                        eprintln!("assertion error");
                        std::process::abort()
                    }
                }
                InsKind::CellSet => {
                    let right = self.stack_pop();
                    let left = match self.stack_pop() {
                        (CellTy::Cell, id) => id as usize,
                        _ => panic!("Type error"),
                    };
                    self.table_set(left, right);
                }
                InsKind::Bool(value) => {
                    self.stack_push((CellTy::Bool, value as i64));
                }
                InsKind::Int(value) => {
                    self.stack_push((CellTy::Int, value));
                }
                InsKind::GlobalGet => {
                    match self.stack_pop() {
                        (CellTy::Int, addr) => {
                            self.stack_push((CellTy::Cell, addr));
                        }
                        _ => panic!("Type error"),
                    }
                }
                InsKind::OpAdd => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => self.stack_push((CellTy::Int, left + right)),
                        _ => panic!("Type error")
                    }
                }
                InsKind::OpSub => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => self.stack_push((CellTy::Int, left - right)),
                        _ => panic!("Type error")
                    }
                }
                InsKind::OpMul => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => self.stack_push((CellTy::Int, left * right)),
                        _ => panic!("Type error")
                    }
                }
                InsKind::OpDiv => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    // FIXME: ゼロ除算をエラーにする
                    match (left, right) {
                        ((CellTy::Int, left), (CellTy::Int, right)) => self.stack_push((CellTy::Int, left / right)),
                        _ => panic!("Type error")
                    }
                }
                InsKind::OpEq => {
                    let right = self.stack_pop_val();
                    let left = self.stack_pop_val();
                    match (left, right) {
                        ((CellTy::Bool, left), (CellTy::Bool, right)) => self.stack_push((CellTy::Bool, (left == right) as i64)),
                        ((CellTy::Int, left), (CellTy::Int, right)) => self.stack_push((CellTy::Bool, (left == right) as i64)),
                        _ => self.stack_push((CellTy::Bool, false as i64)),
                    }
                }
            }
        }
    }
}

pub fn run(il_text: &str) -> ! {
    let il_tree = il::parse::parse(il_text);
    let (inss, global_count) = ins::gen(&il_tree);

    let mut runtime = Runtime::new();
    runtime.inss = inss;
    runtime.heap_alloc(global_count);
    runtime.run()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
