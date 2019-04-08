use super::*;
use crate::pir::{self, Pir, PirKind, PirProgram, PirVal};
use crate::semantics::*;
use crate::syntax::*;
use std::collections::BTreeMap;
use std::fmt::Write;
use std::iter;
use std::mem::size_of;
use std::rc::Rc;

struct GenRir {
    rir: Rir,
    current_fun_id: FunId,
}

impl GenRir {
    fn pir_program(&self) -> &PirProgram {
        &self.rir.pir_program
    }

    fn share_pir_program(&self) -> Rc<PirProgram> {
        Rc::clone(&self.rir.pir_program)
    }

    fn break_label_id(&self, loop_id: LoopId) -> LabelId {
        self.rir.loops[&loop_id].break_label_id
    }

    fn continue_label_id(&self, loop_id: LoopId) -> LabelId {
        self.rir.loops[&loop_id].continue_label_id
    }

    fn current_gen_fun(&self) -> &GenFunDef {
        &self.rir.funs[&self.current_fun_id]
    }

    fn current_gen_fun_mut(&mut self) -> &mut GenFunDef {
        let fun_id = self.current_fun_id;
        self.rir.funs.get_mut(&fun_id).unwrap()
    }

    fn add_reg(&mut self) -> RegId {
        self.rir.reg_count += 1;
        RegId::new(self.rir.reg_count - 1)
    }

    fn add_label(&mut self) -> LabelId {
        self.rir.label_count += 1;
        LabelId::new(self.rir.label_count - 1)
    }

    /// Allocate and write global data. Return the range.
    fn alloc(&mut self, data: &[u8]) -> (usize, usize) {
        let p = self.rir.text.len();
        self.rir.text.extend(data);
        let q = self.rir.text.len();
        (p, q)
    }

    fn alloc_zero(&mut self, size: usize) -> (usize, usize) {
        let data = iter::repeat(0).take(size).collect::<Vec<_>>();
        self.alloc(&data)
    }

    fn kill(&mut self, reg_id: RegId) {
        self.push(Cmd::Kill, reg_id, CmdArg::None);
    }

    fn push(&mut self, cmd: Cmd, l: RegId, r: CmdArg) {
        self.current_gen_fun_mut().inss.push((cmd, l, r));
    }

    fn add_reg_imm(&mut self, value: i64) -> RegId {
        let reg_id = self.add_reg();
        self.push(Cmd::Imm, reg_id, CmdArg::Int(value));
        reg_id
    }

    fn add_cmd_lo32(&mut self, target: RegId) {
        const MASK: i64 = 0xFFFF_FFFF;
        let mask = self.add_reg_imm(MASK);
        self.push(Cmd::BitAnd, target, CmdArg::Reg(mask));
        self.kill(mask);
    }

    fn add_cmd_store(&mut self, dest: RegId, src: RegId, size: usize) {
        let cmd = match size {
            1 => Cmd::Store8,
            8 => Cmd::Store,
            _ => panic!("store of unsized value {:?}", size),
        };
        self.push(cmd, dest, CmdArg::Reg(src));
        self.kill(src);
    }

    fn add_cmd_load(&mut self, dest: RegId, src: RegId, size: usize) {
        let cmd = match size {
            1 => Cmd::Load8,
            8 => Cmd::Load,
            _ => panic!("load of unsized value {:?}", size),
        };
        self.push(cmd, dest, CmdArg::Reg(src));
        self.kill(src);
    }

    fn add_cmd_call_fun(&mut self, args: &[Pir], fun_id: FunId) -> RegId {
        let body_label_id = self.rir.funs[&fun_id].body_label_id;

        // Push args to stack in reversed order.
        for arg in args.iter().rev() {
            let reg_id = self.on_exp(arg);
            self.push(Cmd::Push, reg_id, CmdArg::None);
            self.kill(reg_id);
        }

        self.push(Cmd::Call, NO_REG_ID, CmdArg::Label(body_label_id));

        // Pop args from stack.
        let reg_id = self.add_reg();
        for _ in args {
            self.push(Cmd::Pop, reg_id, CmdArg::None);
        }

        // Copy the result.
        self.push(Cmd::Mov, reg_id, CmdArg::Reg(RET_REG_ID));
        reg_id
    }

    fn on_call_prim(&mut self, prim: Prim, args: &[Pir]) -> RegId {
        match prim {
            Prim::ByteToInt | Prim::IntToByte | Prim::SliceLen => {
                panic!("{:?} should not exit", prim)
            }
            Prim::MemAlloc => {
                let size_reg_id = self.on_exp(&args[0]);
                let ptr_reg_id = self.add_reg();

                self.push(Cmd::Alloc, ptr_reg_id, CmdArg::Reg(size_reg_id));
                self.kill(size_reg_id);
                ptr_reg_id
            }
            Prim::ReadInt => {
                let reg_id = self.add_reg();
                self.push(Cmd::ReadInt, reg_id, CmdArg::None);
                reg_id
            }
            Prim::ReadStr => {
                let reg_id = self.add_reg();
                self.push(Cmd::ReadStr, reg_id, CmdArg::None);
                reg_id
            }
            Prim::PrintLnInt => {
                let r = self.on_exp(&args[0]);
                self.push(Cmd::PrintLnInt, NO_REG_ID, CmdArg::Reg(r));
                self.kill(r);
                NO_REG_ID
            }
            Prim::Print => {
                let ptr_reg_id = self.on_exp(&args[0]);
                self.push(Cmd::Write, ptr_reg_id, CmdArg::None);
                self.kill(ptr_reg_id);
                NO_REG_ID
            }
        }
    }

    fn on_bin(&mut self, op: Op, size: usize, l: &Pir, r: &Pir) -> RegId {
        let l_reg = self.on_exp(l);
        let r_reg = self.on_exp(r);
        let cmd = match op {
            Op::Set => {
                self.add_cmd_lo32(l_reg);
                self.add_cmd_store(l_reg, r_reg, size);
                self.kill(l_reg);
                self.kill(r_reg);
                return NO_REG_ID;
            }
            Op::SetAdd | Op::SetSub | Op::SetMul | Op::SetDiv | Op::SetMod => {
                let op_cmd = match op {
                    Op::SetAdd => Cmd::Add,
                    Op::SetSub => Cmd::Sub,
                    Op::SetMul => Cmd::Mul,
                    Op::SetDiv => Cmd::Div,
                    Op::SetMod => Cmd::Mod,
                    _ => unreachable!(),
                };
                let t_reg = self.add_reg();
                self.add_cmd_load(t_reg, l_reg, size);
                self.push(op_cmd, t_reg, CmdArg::Reg(r_reg));
                self.add_cmd_store(l_reg, t_reg, size);
                self.kill(l_reg);
                self.kill(r_reg);
                self.kill(t_reg);
                return NO_REG_ID;
            }
            Op::Eq => Cmd::Eq,
            Op::Ne => Cmd::Ne,
            Op::Lt => Cmd::Lt,
            Op::Le => Cmd::Le,
            Op::Gt => Cmd::Gt,
            Op::Ge => Cmd::Ge,
            Op::Add => Cmd::Add,
            Op::Sub => Cmd::Sub,
            Op::Mul => Cmd::Mul,
            Op::Div => Cmd::Div,
            Op::Mod => Cmd::Mod,
            Op::BitAnd => Cmd::BitAnd,
            Op::BitOr => Cmd::BitOr,
            Op::BitXor => Cmd::BitXor,
            Op::BitShiftL => Cmd::BitShiftL,
            Op::BitShiftR => Cmd::BitShiftR,
            Op::LogOr => unreachable!("|| is reduced"),
            Op::LogAnd => unreachable!("&& is reduced"),
            Op::Anno => unreachable!("PIR should remove anno"),
            Op::As => unreachable!("PIR should remove as"),
            Op::Range => panic!("Can't generate range operation"),
        };

        self.push(cmd, l_reg, CmdArg::Reg(r_reg));
        self.kill(r_reg);
        l_reg
    }

    fn on_exp(&mut self, pir: &Pir) -> RegId {
        match pir.kind() {
            &PirKind::Val(PirVal::Unit) => NO_REG_ID,
            &PirKind::Val(PirVal::Int(value)) => {
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Int(value));
                reg_id
            }
            &PirKind::Val(PirVal::Byte(value)) => {
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Int(value as i64));
                reg_id
            }
            PirKind::Val(PirVal::Str(value)) => {
                let (p, q) = self.alloc(value.as_bytes());
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Ptr(p, q));
                reg_id
            }
            &PirKind::Var { var_id } => {
                let var_kind = self.pir_program().var_kind(var_id);

                let ptr_reg_id = self.add_reg();
                match var_kind {
                    PirVarKind::Global { index } => {
                        let ptr = (index * size_of::<i64>()) as i64;
                        self.push(Cmd::Imm, ptr_reg_id, CmdArg::Int(ptr));
                    }
                    PirVarKind::Local { index } => {
                        let offset = -((index + 1) as i64) * size_of::<i64>() as i64;
                        self.push(Cmd::Mov, ptr_reg_id, CmdArg::Reg(BASE_PTR_REG_ID));
                        self.push(Cmd::AddImm, ptr_reg_id, CmdArg::Int(offset));
                    }
                    PirVarKind::Arg { index } => {
                        // Before the base pointer, arguments are stacked under register values and return address.
                        let offset =
                            ((index + (REG_NUM - KNOWN_REG_NUM + 2)) * size_of::<i64>()) as i64;
                        self.push(Cmd::Mov, ptr_reg_id, CmdArg::Reg(BASE_PTR_REG_ID));
                        self.push(Cmd::AddImm, ptr_reg_id, CmdArg::Int(offset));
                    }
                };
                ptr_reg_id
            }
            &PirKind::CallPrim(prim) => self.on_call_prim(prim, pir.children()),
            &PirKind::CallFun(fun_id) => self.add_cmd_call_fun(pir.children(), fun_id),
            &PirKind::Op { op, size } => {
                let (l, r) = {
                    let children = pir.children();
                    (&children[0], &children[1])
                };
                self.on_bin(op, size, l, r)
            }
            &PirKind::Deref { size } => {
                let ptr = &pir.children()[0];

                let ptr_reg_id = self.on_exp(ptr);
                let reg_id = self.add_reg();
                self.add_cmd_load(reg_id, ptr_reg_id, size);
                self.kill(ptr_reg_id);

                reg_id
            }
            PirKind::Return => {
                let result = &pir.children()[0];
                let end_label_id = self.current_gen_fun().end_label_id;
                let reg_id = self.on_exp(result);
                self.push(Cmd::Mov, RET_REG_ID, CmdArg::Reg(reg_id));
                self.push(Cmd::Jump, NO_REG_ID, CmdArg::Label(end_label_id));
                self.kill(reg_id);
                NO_REG_ID
            }
            PirKind::If => {
                let (cond, body, alt) = {
                    let children = pir.children();
                    (&children[0], &children[1], &children[2])
                };

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
            &PirKind::While { loop_id } => {
                let (cond, body) = {
                    let children = pir.children();
                    (&children[0], &children[1])
                };

                let break_label = CmdArg::Label(self.break_label_id(loop_id));
                let continue_label = CmdArg::Label(self.continue_label_id(loop_id));

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
            &PirKind::Break { loop_id } => {
                let break_label = CmdArg::Label(self.break_label_id(loop_id));

                self.push(Cmd::Jump, NO_REG_ID, break_label);
                NO_REG_ID
            }
            &PirKind::Continue { loop_id } => {
                let continue_label = CmdArg::Label(self.continue_label_id(loop_id));

                self.push(Cmd::Jump, NO_REG_ID, continue_label);
                NO_REG_ID
            }
            PirKind::Semi => self.on_exps(pir.children()),
            PirKind::IndexPoint | PirKind::IndexSlice => {
                panic!("{:?} should vanish in reduce", pir.kind())
            }
        }
    }

    fn on_exps(&mut self, pirs: &[Pir]) -> RegId {
        let mut reg_id = NO_REG_ID;
        for pir in pirs {
            let result = self.on_exp(pir);
            self.kill(reg_id);
            reg_id = result;
        }
        reg_id
    }

    fn add_cmd_save_caller_regs(&mut self) {
        self.push(Cmd::Push, BASE_PTR_REG_ID, CmdArg::None);
        self.push(Cmd::PushRegs, NO_REG_ID, CmdArg::None);
        self.push(Cmd::Mov, BASE_PTR_REG_ID, CmdArg::Reg(STACK_PTR_REG_ID));
    }

    fn add_cmd_restore_caller_regs(&mut self) {
        self.push(Cmd::PopRegs, NO_REG_ID, CmdArg::None);
        self.push(Cmd::Pop, BASE_PTR_REG_ID, CmdArg::None);
    }

    fn add_cmd_allocate_locals(&mut self, fun_id: FunId) {
        let local_count = self.pir_program().fun_local_count(fun_id);
        let frame_size = (local_count * size_of::<i64>()) as i64;
        if fun_id == GLOBAL_FUN_ID {
            self.alloc_zero(frame_size as usize);
        } else {
            self.push(Cmd::AddImm, STACK_PTR_REG_ID, CmdArg::Int(-frame_size));
        }
    }

    fn add_cmd_deallocate_locals(&mut self, fun_id: FunId) {
        if fun_id == GLOBAL_FUN_ID {
            return;
        }

        let local_count = self.pir_program().fun_local_count(fun_id);
        let frame_size = (local_count * size_of::<i64>()) as i64;
        self.push(Cmd::AddImm, STACK_PTR_REG_ID, CmdArg::Int(frame_size));
    }

    fn add_cmd_fun_end(&mut self, fun_id: FunId) {
        let end_label_id = self.rir.funs[&fun_id].end_label_id;

        self.push(Cmd::Label, NO_REG_ID, CmdArg::Label(end_label_id));

        if fun_id == GLOBAL_FUN_ID {
            self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);
        } else {
            self.add_cmd_deallocate_locals(fun_id);
            self.add_cmd_restore_caller_regs();
            self.push(Cmd::Ret, NO_REG_ID, CmdArg::None);
        }
    }

    pub fn gen_fun(&mut self, fun_id: FunId) {
        self.current_fun_id = fun_id;
        let body_label_id = self.rir.funs[&fun_id].body_label_id;

        self.push(Cmd::Label, NO_REG_ID, CmdArg::Label(body_label_id));
        self.add_cmd_save_caller_regs();
        self.add_cmd_allocate_locals(fun_id);

        let final_reg_id = {
            let pir_program = self.share_pir_program();
            let body = pir_program.fun_body(fun_id);
            self.on_exp(body)
        };
        self.push(Cmd::Mov, RET_REG_ID, CmdArg::Reg(final_reg_id));
        self.kill(final_reg_id);

        self.add_cmd_fun_end(fun_id);
    }

    pub fn compile(&mut self) -> CompilationResult {
        // Allocate well-known registers.
        self.rir.reg_count += KNOWN_REG_NUM;

        // Prepare funs.
        for (fun_id, fun_def) in self.share_pir_program().funs() {
            let body_label_id = self.add_label();
            let end_label_id = self.add_label();
            self.rir.funs.insert(
                fun_id,
                GenFunDef {
                    body_label_id,
                    end_label_id,
                    inss: vec![],
                },
            );

            for loop_id in fun_def.body().collect_loops() {
                let break_label_id = self.add_label();
                let continue_label_id = self.add_label();
                self.rir.loops.insert(
                    loop_id,
                    GenLoopDef {
                        break_label_id,
                        continue_label_id,
                    },
                );
            }
        }

        // Generate funs.
        for (fun_id, _) in self.share_pir_program().funs() {
            self.gen_fun(fun_id);
        }

        // Reassign registers to finite number registers.
        for fun_id in 0..self.rir.funs.len() {
            regalloc::alloc_regs(&mut self.rir.funs.get_mut(&FunId::new(fun_id)).unwrap().inss);
        }

        // Merge instructions.
        let mut inss = vec![];
        for (_, fun) in &self.rir.funs {
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

        // Write rir.
        let mut program = String::new();
        let csv_encoded_text = (self.rir.text.iter().map(|&b| b.to_string()))
            .collect::<Vec<_>>()
            .join(",");
        writeln!(program, "    .text {}", csv_encoded_text).unwrap();
        for &(cmd, l, r) in &inss {
            let cmd = serialize_cmd(cmd);
            writeln!(program, "    {} {} {}", cmd, l, r.to_i64()).unwrap();
        }

        CompilationResult {
            success: true,
            program,
            msgs: vec![],
        }
    }
}

impl CmdArg {
    fn to_i64(self) -> i64 {
        match self {
            CmdArg::None => 0,
            CmdArg::Int(value) => value,
            CmdArg::Ptr(p, q) => (lo32(q) << 32 | lo32(p)) as i64,
            CmdArg::Reg(value) => usize::from(value) as i64,
            CmdArg::Label(value) => usize::from(value) as i64,
        }
    }
}

fn lo32(x: usize) -> usize {
    x & 0xFFFF_FFFF
}

pub(crate) fn gen(program: Rc<PirProgram>) -> CompilationResult {
    let mut compiler = GenRir {
        rir: Rir {
            pir_program: Rc::clone(&program),
            reg_count: 0,
            label_count: 0,
            text: vec![],
            funs: BTreeMap::new(),
            loops: BTreeMap::new(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    compiler.compile()
}

pub(crate) fn compile(src: &str) -> CompilationResult {
    let sema = Rc::new(analyze::analyze_str(src));
    let pir_program = Rc::new(pir::from_sema(sema));
    gen::gen(pir_program)
}
