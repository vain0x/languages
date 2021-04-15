use crate::internals::*;
use crate::xir_gen::*;
use fmt::{write, Write as _};

#[derive(Clone)]
pub(crate) enum Value<'b> {
    Undef,
    Int(i64),
    String(String),
    Fn(&'b XBody<'b>),
}

impl<'b> Debug for Value<'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Undef => f.write_str("{undef}"),
            Value::Int(value) => Debug::fmt(value, f),
            Value::String(value) => Debug::fmt(value, f),
            Value::Fn(body) => write!(f, "{}{}{}", "{", body.name, "}"),
        }
    }
}

pub(crate) struct Frame<'b> {
    pub(crate) body: &'b XBody<'b>,
    pub(crate) locals: HashMap<&'b str, Value<'b>>,
}

pub(crate) struct XirEval<'b> {
    program: &'b XProgram<'b>,
    pub(crate) output: String,

    /// Call stack.
    pub(crate) stack: Vec<Frame<'b>>,
}

impl<'b> XirEval<'b> {
    pub(crate) fn new(program: &'b XProgram<'b>) -> Self {
        XirEval {
            program,
            output: String::new(),
            stack: Vec::with_capacity(1000),
        }
    }

    pub(crate) fn run(&mut self) -> i32 {
        for body in self
            .program
            .modules
            .iter()
            .map(|m| m.toplevel)
            .collect::<Vec<_>>()
        {
            self.stack.push(Frame {
                body,
                locals: body
                    .args
                    .iter()
                    .map(|a| a.text)
                    .chain(body.locals.borrow().iter().map(|local| local.name))
                    .map(|name| (name, Value::Undef))
                    .collect(),
            });

            let blocks = body.blocks.borrow();
            let entry_block = blocks.last().expect("entry block");
            {
                eprintln!("trace: enter {}", body.name);

                let block = entry_block;
                for stmt in &block.stmts {
                    match stmt {
                        XStmt::Debug(stmt) => {
                            eprintln!("trace: debug {:?}", stmt.arg);
                            self.output += "debug: ";

                            match &stmt.arg {
                                XArg::Const(k) => match k {
                                    XConst::Bool(true) => write!(&mut self.output, "true"),
                                    XConst::Bool(false) => write!(&mut self.output, "false"),
                                    XConst::Int(value) => write!(&mut self.output, "{:?}", value),
                                    XConst::String(value) => {
                                        write!(&mut self.output, "{:?}", value)
                                    }
                                },
                                XArg::Local(local) => write!(
                                    &mut self.output,
                                    "{:?}",
                                    &self.stack.last().unwrap().locals[local.name]
                                ),
                            }
                            .unwrap();
                            self.output += "\n";
                        }
                        XStmt::SetImmediateString { target, value } => {
                            eprintln!("trace: {} <- {:?}", target.name, value);

                            *self
                                .stack
                                .last_mut()
                                .unwrap()
                                .locals
                                .get_mut(target.name)
                                .unwrap() = Value::String((*value).to_owned());
                        }

                        XStmt::Copy { target, src } => {
                            eprintln!("trace: {} <- {}", target.name, src.name);

                            let frame = self.stack.last_mut().unwrap();
                            let value = frame.locals[src.name].clone();
                            *frame.locals.get_mut(target.name).unwrap() = value;
                        }
                        XStmt::SetFn { target, body } => {
                            eprintln!("trace: {} <- {}", target.name, body.name);

                            let frame = self.stack.last_mut().unwrap();
                            *frame.locals.get_mut(target.name).unwrap() = Value::Fn(body);
                        }
                    }
                }
                match &block.terminator {
                    XTerminator::Exit => return 0,
                    XTerminator::Return(_) => {}
                }
            }

            eprintln!("trace: leave {}", body.name);
        }

        1
    }
}
