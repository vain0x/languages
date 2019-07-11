use crate::il::*;

pub(crate) mod il;

pub(crate) struct Runtime {
    table: Vec<i64>,
    stack_end: usize,
    program: Vec<Code>,
    pc: usize,
}

impl Runtime {
    fn new() -> Self {
        Runtime {
            table: vec![0xcd; 1024],
            stack_end: 1024,
            program: vec![],
            pc: 0,
        }
    }

    fn add_code(&mut self, code: Code) {
        self.program.push(code);
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
            match self.program[self.pc - 1] {
                Code::Exit => std::process::exit(0),
                Code::Assert => {
                    if self.stack_pop() == 0 {
                        eprintln!("assertion error");
                        std::process::abort()
                    }
                }
                Code::PushTrue => {
                    self.stack_push(1);
                }
                Code::PushInt(value) => {
                    self.stack_push(value);
                }
            }
        }
    }
}

pub fn run(il_text: &str) -> ! {
    let codes = il::parse::parse(il_text);

    let mut runtime = Runtime::new();
    for code in codes {
        runtime.add_code(code);
    }

    runtime.run()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
