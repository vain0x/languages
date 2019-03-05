use crate::cmd::*;
use crate::sema::Sema;
use crate::*;
use std::fmt::Write;

#[derive(Clone, Copy, Debug)]
pub enum CmdArg {
    None,
    Int(i64),
    Reg(RegId),
    Label(LabelId),
}

pub type Ins = (Cmd, RegId, CmdArg);

pub struct Mir {
    pub sema: Rc<Sema>,
    pub ins: Vec<Ins>,
    pub reg_count: usize,
    pub label_count: usize,
    pub msgs: BTreeMap<MsgId, Msg>,
}

struct Compiler {
    mir: Mir,
    current_fun_id: FunId,
}

impl Compiler {
    fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.mir.sema.syntax.exps[&exp_id]
    }

    fn add_reg(&mut self) -> RegId {
        self.mir.reg_count += 1;
        RegId(self.mir.reg_count - 1)
    }

    fn add_label(&mut self) -> LabelId {
        self.mir.label_count += 1;
        LabelId(self.mir.label_count - 1)
    }

    fn kill(&mut self, reg_id: RegId) {
        self.push(Cmd::Kill, reg_id, CmdArg::None);
    }

    fn push(&mut self, cmd: Cmd, l: RegId, r: CmdArg) {
        self.mir.ins.push((cmd, l, r));
    }

    fn on_err(&mut self, exp_id: ExpId, msg_id: MsgId) -> RegId {
        self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);
        NO_REG_ID
    }

    fn on_int(&mut self, exp_id: ExpId, value: i64) -> RegId {
        let l = self.add_reg();
        self.push(Cmd::Imm, l, CmdArg::Int(value));
        l
    }

    fn on_str(&mut self, exp_id: ExpId, value: &str) -> RegId {
        unimplemented!()
    }

    fn on_ident(&mut self, exp_id: ExpId, name: &str) -> RegId {
        unimplemented!()
    }

    fn on_call(&mut self, exp_id: ExpId, callee: ExpId, args: &[ExpId]) -> RegId {
        let r = self.on_exp(args[0]);
        self.push(Cmd::PrintLnInt, NO_REG_ID, CmdArg::Reg(r));
        self.kill(r);
        NO_REG_ID
    }

    fn on_bin(&mut self, exp_id: ExpId, op: Op, exp_l: ExpId, exp_r: ExpId) -> RegId {
        let l = self.on_exp(exp_l);
        let r = self.on_exp(exp_r);
        self.push(Cmd::Add, l, CmdArg::Reg(r));
        self.kill(r);
        l
    }

    fn on_let(&mut self, exp_id: ExpId, pat: ExpId, init: ExpId) -> RegId {
        unimplemented!()
    }

    fn on_semi(&mut self, exp_id: ExpId, exps: &[ExpId]) -> RegId {
        unimplemented!()
    }

    fn on_exp(&mut self, exp_id: ExpId) -> RegId {
        let syntax = Rc::clone(&self.mir.sema.syntax);
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            &ExpKind::Err(msg_id) => self.on_err(exp_id, msg_id),
            &ExpKind::Int(value) => self.on_int(exp_id, value),
            ExpKind::Str(value) => self.on_str(exp_id, value),
            ExpKind::Ident(name) => self.on_ident(exp_id, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, *callee, &args),
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, op, l, r),
            &ExpKind::Let { pat, init } => self.on_let(exp_id, pat, init),
            ExpKind::Semi(exps) => self.on_semi(exp_id, &exps),
        }
    }

    pub fn compile(&mut self) -> CompilationResult {
        // Allocate well-known registers.
        self.mir.reg_count += KNOWN_REG_NUM;

        let final_reg_id = self.on_exp(self.mir.sema.syntax.root_exp_id);
        self.kill(final_reg_id);
        self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);

        // Write MIR.
        let mut program = String::new();
        for &(cmd, l, r) in &self.mir.ins {
            let cmd = crate::serialize_cmd(cmd);
            writeln!(program, "    {} {} {}", cmd, l.0, r.to_i64()).unwrap();
        }

        // Emit compile errors.
        let mut success = true;
        let mut stderr = String::new();
        for (_, msg) in &self.mir.msgs {
            let ((ly, lx), (ry, rx)) = self.mir.sema.syntax.locate_exp(msg.exp_id);
            writeln!(
                stderr,
                "At {}:{}..{}:{} {}",
                1 + ly,
                1 + lx,
                1 + ry,
                1 + rx,
                msg.message
            )
            .unwrap();
            success = success && msg.level != MsgLevel::Err;
        }

        CompilationResult {
            success,
            program,
            stderr,
        }
    }
}

impl CmdArg {
    fn to_i64(self) -> i64 {
        match self {
            CmdArg::None => 0,
            CmdArg::Int(value) => value,
            CmdArg::Reg(value) => value.0 as i64,
            CmdArg::Label(value) => value.0 as i64,
        }
    }
}

pub fn gen_mir(sema: Rc<Sema>) -> CompilationResult {
    let mut compiler = Compiler {
        mir: Mir {
            sema: Rc::clone(&sema),
            ins: vec![],
            reg_count: 0,
            label_count: 0,
            msgs: sema.msgs.clone(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    compiler.compile()
}
