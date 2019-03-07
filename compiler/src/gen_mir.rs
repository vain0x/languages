use crate::cmd::*;
use crate::*;
use std::fmt::Write;
use std::mem::size_of;

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

    fn current_gen_fun_mut(&mut self) -> &mut GenFunDef {
        let fun_id = self.current_fun_id;
        self.mir.funs.get_mut(&fun_id).unwrap()
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
        self.current_gen_fun_mut().inss.push((cmd, l, r));
    }

    pub fn gen_fun(&mut self, fun_id: FunId) {
        let label_id = self.add_label();

        self.mir.funs.insert(
            fun_id,
            GenFunDef {
                label_id,
                inss: vec![],
            },
        );

        self.push(Cmd::Label, NO_REG_ID, CmdArg::Label(label_id));

        // Allocate local variables.
        let local_count = self.sema().funs[&fun_id].locals.len();
        let frame_size = (local_count * size_of::<i64>()) as i64;
        self.push(Cmd::AddImm, BASE_PTR_REG_ID, CmdArg::Int(-frame_size));

        let final_reg_id = self.on_exp(self.mir.sema.funs[&fun_id].body);
        self.kill(final_reg_id);
        self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);
    }

    pub fn compile(&mut self) -> CompilationResult {
        // Allocate well-known registers.
        self.mir.reg_count += KNOWN_REG_NUM;

        self.gen_fun(GLOBAL_FUN_ID);

        // Reassign registers to finite number registers.
        for fun_id in 0..self.mir.funs.len() {
            regalloc::alloc_regs(&mut self.mir.funs.get_mut(&FunId(fun_id)).unwrap().inss);
        }

        // Merge instructions.
        let mut inss = vec![];
        for (_, fun) in &self.mir.funs {
            inss.extend(&fun.inss);
        }

        // Build jump table.
        let mut labels = BTreeMap::new();
        for pc in 0..inss.len() {
            if let (Cmd::Label, _, CmdArg::Label(r)) = inss[pc] {
                labels.insert(r, pc);
            }
        }

        // Replace label ids with pc.
        for i in 0..inss.len() {
            if let CmdArg::Label(label_id) = inss[i].2 {
                inss[i].2 = CmdArg::Int(labels[&label_id] as i64);
            }
        }

        // Write MIR.
        let mut program = String::new();
        for &(cmd, l, r) in &inss {
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

impl ShareSyntax for Compiler {
    fn share_syntax(&self) -> Rc<Syntax> {
        Rc::clone(&self.sema().syntax)
    }
}

impl ExpVisitor for Compiler {
    type Output = RegId;

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
                    self.kill(offset_reg_id);
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
            Op::Eq => Cmd::Eq,
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

    fn on_if(&mut self, _: ExpId, cond: ExpId, body: ExpId, alt: ExpId) -> RegId {
        let alt_label = CmdArg::Label(self.add_label());
        let end_label = CmdArg::Label(self.add_label());
        let end_reg = self.add_reg();

        let cond_reg_id = self.on_exp(cond);
        self.push(Cmd::Unless, cond_reg_id, alt_label);
        self.kill(cond_reg_id);

        let body_reg_id = self.on_exp(body);
        self.push(Cmd::Mov, end_reg, CmdArg::Reg(body_reg_id));
        self.kill(body_reg_id);
        self.push(Cmd::Jump, NO_REG_ID, end_label);

        self.push(Cmd::Label, NO_REG_ID, alt_label);
        let alt_reg_id = self.on_exp(alt);
        self.push(Cmd::Mov, end_reg, CmdArg::Reg(alt_reg_id));
        self.kill(alt_reg_id);

        self.push(Cmd::Label, NO_REG_ID, end_label);
        end_reg
    }

    fn on_while(&mut self, _: ExpId, cond: ExpId, body: ExpId) -> RegId {
        let continue_label = CmdArg::Label(self.add_label());
        let break_label = CmdArg::Label(self.add_label());

        self.push(Cmd::Label, NO_REG_ID, continue_label);
        let cond_reg_id = self.on_exp(cond);
        self.push(Cmd::Unless, cond_reg_id, break_label);
        self.kill(cond_reg_id);

        let body_reg_id = self.on_exp(body);
        self.kill(body_reg_id);
        self.push(Cmd::Jump, NO_REG_ID, continue_label);

        self.push(Cmd::Label, NO_REG_ID, break_label);
        NO_REG_ID
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
            reg_count: 0,
            label_count: 0,
            funs: BTreeMap::new(),
            msgs: sema.msgs.clone(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    compiler.compile()
}
