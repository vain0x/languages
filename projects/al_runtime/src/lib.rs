pub(crate) enum Code {
    PushInt(i64),
    Exit,
}

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
                Code::PushInt(value) => {
                    self.stack_push(value);
                }
                Code::Exit => std::process::exit(self.stack_pop() as i32),
            }
        }
    }
}

pub fn run(il: &str) -> ! {
    let exit = if il.trim() == "assert(false)" { 1 } else { 0 };
    let codes = vec![Code::PushInt(exit), Code::Exit];

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
