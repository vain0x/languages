use super::*;
use std::fmt::Write as _;

struct JsPrinter {
    out: String,
    indent: usize,
}

fn js_op_to_str(op: JsOp) -> &'static str {
    match op {
        JsOp::Set => "=",
        JsOp::SetAdd => "+=",
        JsOp::SetSub => "-=",
        JsOp::SetMul => "*=",
        JsOp::SetDiv => "/=",
        JsOp::Eq => "===",
        JsOp::Ne => "!==",
        JsOp::Lt => "<",
        JsOp::Le => "<=",
        JsOp::Gt => ">",
        JsOp::Ge => ">=",
        JsOp::Add => "+",
        JsOp::Sub => "-",
        JsOp::BitOr => "|",
        JsOp::BitXor => "^",
        JsOp::Mul => "*",
        JsOp::Div => "/", // FIXME: int div
        JsOp::Mod => "%",
        JsOp::BitAnd => "&",
        JsOp::BitShiftL => "<<",
        JsOp::BitShiftR => ">>",
    }
}
impl JsPrinter {
    fn print_indent(&mut self) {
        for _ in 0..self.indent * 2 {
            self.out.push(' ');
        }
    }

    fn print_exp(&mut self, exp: &JsExp) {
        match exp {
            JsExp::Val(JsVal::Null) => {
                write!(self.out, "null").unwrap();
            }
            JsExp::Val(JsVal::Num(value)) => {
                write!(self.out, "{}", value).unwrap();
            }
            JsExp::Val(JsVal::Str(value)) => {
                write!(self.out, "\"{}\"", value.replace("\n", "\\n")).unwrap();
            }
            JsExp::Val(JsVal::Prim(JsPrim::Print)) => {
                write!(self.out, "process.stdout.write").unwrap();
            }
            JsExp::Val(JsVal::Prim(JsPrim::PrintLn)) => {
                write!(self.out, "console.log").unwrap();
            }
            JsExp::Var { var_id } => {
                write!(self.out, "v{}", var_id).unwrap();
            }
            JsExp::Bin { op, args } => {
                write!(self.out, "(").unwrap();
                self.print_exp(&args[0]);
                write!(self.out, " {} ", js_op_to_str(*op)).unwrap();
                self.print_exp(&args[1]);
                write!(self.out, ")").unwrap();
            }
            JsExp::Call { args } => {
                self.print_exp(&args[0]);
                write!(self.out, "(").unwrap();
                for (i, arg) in args[1..].iter().enumerate() {
                    if i > 0 {
                        write!(self.out, ", ").unwrap();
                    }
                    self.print_exp(arg);
                }
                write!(self.out, ")").unwrap();
            }
            JsExp::Fun { params, body } => {
                write!(self.out, "(").unwrap();
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.out, ", ").unwrap();
                    }
                    write!(self.out, "v{}", param).unwrap();
                }
                write!(self.out, ") => ").unwrap();
                self.print_block(body);
            }
        }
    }

    fn print_stms(&mut self, stms: &[JsStm]) {
        for stm in stms {
            self.print_stm(stm);
        }
    }

    /// Don't end with line break.
    fn print_block(&mut self, stms: &[JsStm]) {
        write!(self.out, "{{\n").unwrap();
        self.indent += 1;
        self.print_stms(stms);
        self.indent -= 1;
        self.print_indent();
        write!(self.out, "}}").unwrap();
    }

    /// Print a statement with indent and end with line break.
    fn print_stm(&mut self, stm: &JsStm) {
        self.print_indent();
        match stm {
            JsStm::Exp(body) => {
                self.print_exp(body);
                write!(self.out, ";\n").unwrap();
            }
            JsStm::Let { pat, body } => {
                write!(self.out, "let v{} = ", pat).unwrap();
                self.print_exp(body);
                write!(self.out, ";\n").unwrap();
            }
            JsStm::If { cond, body, alt } => {
                write!(self.out, "if (").unwrap();
                self.print_exp(&cond);
                write!(self.out, ") ").unwrap();
                self.print_block(&body);
                write!(self.out, " else ").unwrap();
                self.print_block(&alt);
                write!(self.out, "\n").unwrap();
            }
            JsStm::Loop { body } => {
                write!(self.out, "while (true) ").unwrap();
                self.print_block(&body);
                write!(self.out, "\n").unwrap();
            }
            JsStm::Break => {
                write!(self.out, "break;\n").unwrap();
            }
            JsStm::Continue => {
                write!(self.out, "continue;\n").unwrap();
            }
            JsStm::Return(body) => {
                write!(self.out, "return ").unwrap();
                self.print_exp(&body);
                write!(self.out, ";\n").unwrap();
            }
        }
    }

    pub(crate) fn print(&mut self, stms: &[JsStm]) {
        self.print_stms(stms);
    }
}

pub(crate) fn js_print(program: &JsProgram) -> String {
    let mut printer = JsPrinter {
        out: String::new(),
        indent: 0,
    };
    printer.print(&program.stms);
    printer.out
}

pub(crate) fn convert_to_javascript(src: &str) -> String {
    use crate::pir;
    use crate::semantics::analyze;
    use std::rc::Rc;

    let sema = Rc::new(analyze::analyze_str(src));
    let pir_program = pir::from_sema(sema);
    let js_program = super::gen::js_gen(&pir_program);
    js_print(&js_program)
}
