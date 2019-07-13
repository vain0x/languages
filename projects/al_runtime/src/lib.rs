use crate::ins::*;

pub(crate) mod il;
pub(crate) mod ins;

pub(crate) struct Runtime {
    table: Vec<i64>,
    stack_end: usize,
    inss: Vec<InsKind>,
    pc: usize,
}

impl Runtime {
    fn new() -> Self {
        Runtime {
            table: vec![0xcd; 1024],
            stack_end: 1024,
            inss: vec![],
            pc: 0,
        }
    }

    fn stack_push(&mut self, value: i64) {
        self.stack_end -= 1;
        self.table[self.stack_end] = value;
    }

    fn stack_pop(&mut self) -> i64 {
        self.stack_end += 1;
        self.table[self.stack_end - 1]
    }

    fn run(&mut self) -> ! {
        loop {
            self.pc += 1;
            match self.inss[self.pc - 1] {
                InsKind::Exit => std::process::exit(0),
                InsKind::Assert => {
                    if self.stack_pop() == 0 {
                        eprintln!("assertion error");
                        std::process::abort()
                    }
                }
                InsKind::Bool(value) => {
                    self.stack_push(if value { 1 } else { 0 });
                }
                InsKind::Int(value) => {
                    self.stack_push(value);
                }
                InsKind::OpAdd => {
                    let right = self.stack_pop();
                    let left = self.stack_pop();
                    self.stack_push(left + right);
                }
                InsKind::OpSub => {
                    let right = self.stack_pop();
                    let left = self.stack_pop();
                    self.stack_push(left - right);
                }
                InsKind::OpMul => {
                    let right = self.stack_pop();
                    let left = self.stack_pop();
                    self.stack_push(left * right);
                }
                InsKind::OpDiv => {
                    let right = self.stack_pop();
                    let left = self.stack_pop();
                    // FIXME: ゼロ除算をエラーにする
                    self.stack_push(left / right);
                }
                InsKind::OpEq => {
                    let right = self.stack_pop();
                    let left = self.stack_pop();
                    self.stack_push(if left == right { 1 } else { 0 });
                }
            }
        }
    }
}

pub fn run(il_text: &str) -> ! {
    let il_tree = il::parse::parse(il_text);
    let inss = ins::gen(&il_tree);

    let mut runtime = Runtime::new();
    runtime.inss = inss;
    runtime.run()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
