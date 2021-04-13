use crate::xir_gen::*;

pub(crate) struct XirEval<'b> {
    program: &'b XProgram<'b>,
    pub(crate) output: String,
}

impl<'b> XirEval<'b> {
    pub(crate) fn new(program: &'b XProgram<'b>) -> Self {
        XirEval {
            program,
            output: String::new(),
        }
    }

    pub(crate) fn run(&mut self) {
        for body in &self.program.bodies {
            let blocks = body.blocks.borrow();
            for block in blocks.iter() {
                for stmt in &block.stmts {
                    match stmt {
                        XStmt::Debug(stmt) => {
                            self.output += &format!("debug: {:?}\n", stmt.arg);
                        }
                        XStmt::SetImmediateString { target, value } => {
                            // FIXME:
                        }
                    }
                }
            }
        }
    }
}
