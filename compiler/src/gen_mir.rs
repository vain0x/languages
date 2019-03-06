use crate::cmd::*;
use crate::sema::Sema;
use crate::*;
use std::fmt::Write;
use std::mem::size_of;

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
    fn syntax(&self) -> &Syntax {
        &self.sema().syntax
    }

    fn sema(&self) -> &Sema {
        &self.mir.sema
    }

    fn exp(&self, exp_id: ExpId) -> &Exp {
        &self.mir.sema.syntax.exps[&exp_id]
    }

    fn get_symbol(&self, exp_id: ExpId) -> Option<&Symbol> {
        (self.mir.sema.exp_symbols.get(&exp_id)).map(|symbol_id| &self.mir.sema.symbols[&symbol_id])
    }

    fn is_lval(&self, exp_id: ExpId) -> bool {
        self.mir.sema.pats.contains(&exp_id)
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

    fn on_err(&mut self, _: ExpId, _: MsgId) -> RegId {
        self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);
        NO_REG_ID
    }

    fn on_int(&mut self, _: ExpId, value: i64) -> RegId {
        let l = self.add_reg();
        self.push(Cmd::Imm, l, CmdArg::Int(value));
        l
    }

    fn on_str(&mut self, _: ExpId, _: &str) -> RegId {
        unimplemented!()
    }

    fn on_ident(&mut self, exp_id: ExpId, _: &str) -> RegId {
        let symbol = self.get_symbol(exp_id).expect("ident should be resolved");
        match &symbol.kind {
            SymbolKind::Prim(..) => panic!("cannot generate primitive"),
            SymbolKind::Local { index } => {
                let offset = (*index * size_of::<i64>()) as i64;

                let offset_reg_id = self.add_reg();
                self.push(Cmd::Mov, offset_reg_id, CmdArg::Reg(BASE_PTR_REG_ID));
                self.push(Cmd::AddImm, offset_reg_id, CmdArg::Int(offset));

                if self.is_lval(exp_id) {
                    offset_reg_id
                } else {
                    let reg_id = self.add_reg();
                    self.push(Cmd::Load, reg_id, CmdArg::Reg(offset_reg_id));
                    reg_id
                }
            }
        }
    }

    fn on_call(&mut self, _: ExpId, callee: ExpId, args: &[ExpId]) -> RegId {
        let symbol = &self.get_symbol(callee).expect("cannot call non-symbol");
        match symbol.kind {
            SymbolKind::Prim(Prim::ReadInt) => {
                let l = self.add_reg();
                self.push(Cmd::ReadInt, l, CmdArg::None);
                l
            }
            SymbolKind::Prim(Prim::PrintLnInt) => {
                let r = self.on_exp(args[0]);
                self.push(Cmd::PrintLnInt, NO_REG_ID, CmdArg::Reg(r));
                self.kill(r);
                NO_REG_ID
            }
            _ => panic!("cannot call non-primitive symbol"),
        }
    }

    fn on_bin(&mut self, _: ExpId, op: Op, exp_l: ExpId, exp_r: ExpId) -> RegId {
        let l_reg = self.on_exp(exp_l);
        let r_reg = self.on_exp(exp_r);
        let cmd = match op {
            Op::Set => unimplemented!(),
            Op::Eq => unimplemented!(),
            Op::Add => Cmd::Add,
            Op::Sub => Cmd::Sub,
            Op::Mul => Cmd::Mul,
            Op::Div => Cmd::Div,
            Op::Mod => Cmd::Mod,
        };

        self.push(cmd, l_reg, CmdArg::Reg(r_reg));
        self.kill(r_reg);
        l_reg
    }

    fn on_let(&mut self, _: ExpId, pat: ExpId, init: ExpId) -> RegId {
        let init_reg_id = self.on_exp(init);
        let var_reg_id = self.on_exp(pat);
        self.push(Cmd::Store, init_reg_id, CmdArg::Reg(var_reg_id));
        self.kill(var_reg_id);
        self.kill(init_reg_id);
        NO_REG_ID
    }

    fn on_semi(&mut self, _: ExpId, exps: &[ExpId]) -> RegId {
        let mut reg_id = NO_REG_ID;
        for &exp_id in exps {
            let result = self.on_exp(exp_id);
            self.kill(reg_id);
            reg_id = result;
        }
        reg_id
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

        // Allocate main local variables.
        let size = (self.sema().local_count * size_of::<i64>()) as i64;
        self.push(Cmd::AddImm, BASE_PTR_REG_ID, CmdArg::Int(-size));

        let final_reg_id = self.on_exp(self.syntax().root_exp_id);
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
