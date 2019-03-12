use crate::*;
use std::fmt::Write;
use std::mem::size_of;

struct Compiler {
    mir: Mir,
    current_fun_id: FunId,
}

impl Compiler {
    fn sema(&self) -> &Sema {
        &self.mir.sema
    }

    fn current_gen_fun_mut(&mut self) -> &mut GenFunDef {
        let fun_id = self.current_fun_id;
        self.mir.funs.get_mut(&fun_id).unwrap()
    }

    fn get_symbol(&self, exp_id: ExpId) -> Option<SymbolRef<'_>> {
        (self.mir.sema.exp_symbols.get(&exp_id)).map(|&symbol| self.mir.sema.get_symbol_ref(symbol))
    }

    fn is_coerced_to_val(&self, exp_id: ExpId) -> bool {
        self.mir.sema.exp_vals.contains(&exp_id)
    }

    fn get_ty(&self, exp_id: ExpId) -> Ty {
        self.mir.sema.get_ty(exp_id).to_owned()
    }

    fn add_reg(&mut self) -> RegId {
        self.mir.reg_count += 1;
        RegId::new(self.mir.reg_count - 1)
    }

    fn add_label(&mut self) -> LabelId {
        self.mir.label_count += 1;
        LabelId::new(self.mir.label_count - 1)
    }

    /// Allocate and write global data. Return the range.
    fn alloc(&mut self, data: &[u8]) -> (usize, usize) {
        let p = self.mir.text.len();
        self.mir.text.extend(data);
        let q = self.mir.text.len();
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

    fn add_cmd_lo32(&mut self, target: RegId) {
        const MASK: i64 = 0xFFFF_FFFF;
        let mask = self.add_reg();
        self.push(Cmd::Imm, mask, CmdArg::Int(MASK));
        self.push(Cmd::BitAnd, target, CmdArg::Reg(mask));
        self.kill(mask);
    }

    fn add_cmd_hi32(&mut self, target: RegId) {
        let offset = self.add_reg();
        self.push(Cmd::Imm, offset, CmdArg::Int(32));
        self.push(Cmd::BitShiftR, target, CmdArg::Reg(offset));
        self.kill(offset);
    }

    fn on_ident(&mut self, exp_id: ExpId, _: &str) -> RegId {
        let symbol_ref = self.get_symbol(exp_id).expect("ident should be resolved");
        match symbol_ref {
            SymbolRef::Prim(..) => panic!("cannot generate primitive"),
            SymbolRef::Var(_, &VarDef { kind, .. }) => {
                let ptr_reg_id = self.add_reg();
                match kind {
                    VarKind::Global { index } => {
                        let ptr = (index * size_of::<i64>()) as i64;
                        self.push(Cmd::Imm, ptr_reg_id, CmdArg::Int(ptr));
                    }
                    VarKind::Local { index } => {
                        let offset = -((index + 1) as i64) * size_of::<i64>() as i64;
                        self.push(Cmd::Mov, ptr_reg_id, CmdArg::Reg(BASE_PTR_REG_ID));
                        self.push(Cmd::AddImm, ptr_reg_id, CmdArg::Int(offset));
                    }
                    VarKind::Arg { index } => {
                        // Before the base pointer, arguments are stacked under register values and return address.
                        let offset =
                            ((index + (REG_NUM - KNOWN_REG_NUM + 2)) * size_of::<i64>()) as i64;
                        self.push(Cmd::Mov, ptr_reg_id, CmdArg::Reg(BASE_PTR_REG_ID));
                        self.push(Cmd::AddImm, ptr_reg_id, CmdArg::Int(offset));
                    }
                };

                if self.is_coerced_to_val(exp_id) {
                    let reg_id = self.add_reg();
                    self.push(Cmd::Load, reg_id, CmdArg::Reg(ptr_reg_id));
                    self.kill(ptr_reg_id);
                    reg_id
                } else {
                    ptr_reg_id
                }
            }
            SymbolRef::Fun(..) => panic!("cannot generate function"),
        }
    }

    fn on_call(&mut self, _: ExpId, callee: ExpId, args: &[ExpId]) -> RegId {
        let symbol_ref = &self.get_symbol(callee).expect("cannot call non-symbol");
        match symbol_ref {
            SymbolRef::Prim(Prim::ByteToInt) | SymbolRef::Prim(Prim::IntToByte) => {
                self.on_exp(args[0])
            }
            SymbolRef::Prim(Prim::SliceLen) => {
                // len = hi32 - lo32 (in bytes)
                let reg_id = self.on_exp(args[0]);
                let hi_reg_id = self.add_reg();
                let lo_reg_id = self.add_reg();
                self.push(Cmd::Mov, hi_reg_id, CmdArg::Reg(reg_id));
                self.add_cmd_hi32(hi_reg_id);
                self.push(Cmd::Mov, lo_reg_id, CmdArg::Reg(reg_id));
                self.add_cmd_lo32(lo_reg_id);
                self.push(Cmd::Sub, hi_reg_id, CmdArg::Reg(lo_reg_id));
                self.kill(reg_id);
                self.kill(lo_reg_id);
                hi_reg_id
            }
            SymbolRef::Prim(Prim::MemAlloc) => {
                let size_reg_id = self.on_exp(args[0]);
                let ptr_reg_id = self.add_reg();
                self.push(Cmd::Alloc, ptr_reg_id, CmdArg::Reg(size_reg_id));
                self.kill(size_reg_id);
                ptr_reg_id
            }
            SymbolRef::Prim(Prim::ReadInt) => {
                let reg_id = self.add_reg();
                self.push(Cmd::ReadInt, reg_id, CmdArg::None);
                reg_id
            }
            SymbolRef::Prim(Prim::ReadStr) => {
                let reg_id = self.add_reg();
                self.push(Cmd::ReadStr, reg_id, CmdArg::None);
                reg_id
            }
            SymbolRef::Prim(Prim::PrintLnInt) => {
                let r = self.on_exp(args[0]);
                self.push(Cmd::PrintLnInt, NO_REG_ID, CmdArg::Reg(r));
                self.kill(r);
                NO_REG_ID
            }
            SymbolRef::Prim(Prim::Print) => {
                let ptr_reg_id = self.on_exp(args[0]);
                self.push(Cmd::Write, ptr_reg_id, CmdArg::None);
                self.kill(ptr_reg_id);
                NO_REG_ID
            }
            SymbolRef::Fun(fun_id, _) => {
                let body_label_id = self.mir.funs[&fun_id].label_id;

                // Push args to stack in reversed order.
                for arg in args.iter().cloned().rev() {
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
            SymbolRef::Var(..) => panic!("cannot call non-primitive symbol"),
        }
    }

    fn on_bin(&mut self, _: ExpId, op: Op, exp_l: ExpId, exp_r: ExpId) -> RegId {
        let l_reg = self.on_exp(exp_l);
        let r_reg = self.on_exp(exp_r);
        let cmd = match op {
            Op::Set => {
                let size = self.get_ty(exp_l).size_of();
                let cmd = match size {
                    Some(1) => Cmd::Store8,
                    Some(8) => Cmd::Store,
                    _ => unimplemented!(),
                };
                self.add_cmd_lo32(l_reg);
                self.push(cmd, l_reg, CmdArg::Reg(r_reg));
                self.kill(l_reg);
                self.kill(r_reg);
                return NO_REG_ID;
            }
            Op::SetAdd => {
                let t_reg = self.add_reg();
                self.push(Cmd::Load, t_reg, CmdArg::Reg(l_reg));
                self.push(Cmd::Add, t_reg, CmdArg::Reg(r_reg));
                self.push(Cmd::Store, l_reg, CmdArg::Reg(t_reg));
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
        };

        self.push(cmd, l_reg, CmdArg::Reg(r_reg));
        self.kill(r_reg);
        l_reg
    }

    fn on_exp(&mut self, exp_id: ExpId) -> RegId {
        let syntax = self.share_syntax();
        let exp = &syntax.exps[&exp_id];
        match &exp.kind {
            &ExpKind::Err(_) => {
                self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);
                NO_REG_ID
            }
            &ExpKind::Int(value) => {
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Int(value));
                reg_id
            }
            ExpKind::Str(value) => {
                let (p, q) = self.alloc(value.as_bytes());
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Ptr(p, q));
                reg_id
            }
            ExpKind::Ident(name) => self.on_ident(exp_id, name),
            ExpKind::Call { callee, args } => self.on_call(exp_id, *callee, args),
            &ExpKind::Index { indexee, arg } => {
                let indexee_reg_id = self.on_exp(indexee);
                let arg_reg_id = self.on_exp(arg);

                self.push(Cmd::Add, arg_reg_id, CmdArg::Reg(indexee_reg_id));
                self.kill(indexee_reg_id);

                if self.is_coerced_to_val(exp_id) {
                    let byte_reg_id = self.add_reg();
                    self.add_cmd_lo32(arg_reg_id); // begin of slice
                    self.push(Cmd::Load8, byte_reg_id, CmdArg::Reg(arg_reg_id));
                    self.kill(arg_reg_id);
                    byte_reg_id
                } else {
                    arg_reg_id
                }
            }
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, op, l, r),
            ExpKind::Fun { .. } => NO_REG_ID,
            &ExpKind::If { cond, body, alt } => {
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
            &ExpKind::While { cond, body } => {
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
            &ExpKind::Let { pat, init } => {
                let init_reg_id = self.on_exp(init);
                let var_reg_id = self.on_exp(pat);
                self.push(Cmd::Store, var_reg_id, CmdArg::Reg(init_reg_id));
                self.kill(var_reg_id);
                self.kill(init_reg_id);
                NO_REG_ID
            }
            ExpKind::Semi(exps) => {
                let mut reg_id = NO_REG_ID;
                for &exp_id in exps {
                    let result = self.on_exp(exp_id);
                    self.kill(reg_id);
                    reg_id = result;
                }
                reg_id
            }
        }
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
        let local_count = self.sema().fun_local_count(fun_id);
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

        let local_count = self.sema().fun_local_count(fun_id);
        let frame_size = (local_count * size_of::<i64>()) as i64;
        self.push(Cmd::AddImm, STACK_PTR_REG_ID, CmdArg::Int(frame_size));
    }

    fn add_cmd_return(&mut self, fun_id: FunId, result: RegId) {
        if fun_id == GLOBAL_FUN_ID {
            self.push(Cmd::Exit, NO_REG_ID, CmdArg::None);
        } else {
            self.push(Cmd::Mov, RET_REG_ID, CmdArg::Reg(result));
            self.add_cmd_deallocate_locals(fun_id);
            self.add_cmd_restore_caller_regs();
            self.push(Cmd::Ret, NO_REG_ID, CmdArg::None);
        }
    }

    pub fn gen_fun(&mut self, fun_id: FunId) {
        let label_id = self.mir.funs[&fun_id].label_id;

        self.push(Cmd::Label, NO_REG_ID, CmdArg::Label(label_id));
        self.add_cmd_save_caller_regs();
        self.add_cmd_allocate_locals(fun_id);
        let final_reg_id = self.on_exp(self.mir.sema.funs[&fun_id].body);
        self.add_cmd_return(fun_id, final_reg_id);
        self.kill(final_reg_id);
    }

    pub fn compile(&mut self) -> CompilationResult {
        // Allocate well-known registers.
        self.mir.reg_count += KNOWN_REG_NUM;

        // Prepare funs.
        let fun_ids = self.sema().funs.keys().cloned().collect::<Vec<_>>();
        for &fun_id in &fun_ids {
            let label_id = self.add_label();
            self.mir.funs.insert(
                fun_id,
                GenFunDef {
                    label_id,
                    inss: vec![],
                },
            );
        }

        // Generate funs.
        for &fun_id in &fun_ids {
            self.gen_fun(fun_id);
        }

        // Reassign registers to finite number registers.
        for fun_id in 0..self.mir.funs.len() {
            regalloc::alloc_regs(&mut self.mir.funs.get_mut(&FunId::new(fun_id)).unwrap().inss);
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
        let csv_encoded_text = (self.mir.text.iter().map(|&b| b.to_string()))
            .collect::<Vec<_>>()
            .join(",");
        writeln!(program, "    .text {}", csv_encoded_text).unwrap();
        for &(cmd, l, r) in &inss {
            let cmd = crate::serialize_cmd(cmd);
            writeln!(program, "    {} {} {}", cmd, l.0, r.to_i64()).unwrap();
        }

        // Emit compile errors.
        let (success, stderr) = Msg::summarize(self.mir.msgs.values(), &self.mir.sema.syntax);

        CompilationResult {
            success,
            program,
            stderr,
            msgs: self.mir.msgs.values().cloned().collect(),
        }
    }
}

impl ShareSyntax for Compiler {
    fn share_syntax(&self) -> Rc<Syntax> {
        Rc::clone(&self.sema().syntax)
    }
}

impl CmdArg {
    fn to_i64(self) -> i64 {
        match self {
            CmdArg::None => 0,
            CmdArg::Int(value) => value,
            CmdArg::Ptr(p, q) => (lo32(q) << 32 | lo32(p)) as i64,
            CmdArg::Reg(value) => value.0 as i64,
            CmdArg::Label(value) => value.0 as i64,
        }
    }
}

fn lo32(x: usize) -> usize {
    x & 0xFFFF_FFFF
}

pub(crate) fn gen_mir(sema: Rc<Sema>) -> CompilationResult {
    let mut compiler = Compiler {
        mir: Mir {
            sema: Rc::clone(&sema),
            reg_count: 0,
            label_count: 0,
            text: vec![],
            funs: BTreeMap::new(),
            msgs: sema.msgs.clone(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    compiler.compile()
}

pub fn compile(src: &str) -> CompilationResult {
    let doc = Rc::new(Doc::new("main".to_string(), src.to_string()));
    let syntax = Rc::new(parse::parse(doc));

    let sema = Rc::new(sema::sema(syntax));
    if !sema.is_successful() {
        let (success, stderr) = Msg::summarize(sema.msgs.values(), &sema.syntax);
        return CompilationResult {
            success,
            stderr,
            program: "".to_string(),
            msgs: sema.msgs.values().cloned().collect(),
        };
    }

    gen_mir::gen_mir(sema)
}
