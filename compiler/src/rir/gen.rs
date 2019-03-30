use super::*;
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
    fn sema(&self) -> &Sema {
        &self.rir.sema
    }

    fn current_gen_fun(&self) -> &GenFunDef {
        &self.rir.funs[&self.current_fun_id]
    }

    fn current_gen_fun_mut(&mut self) -> &mut GenFunDef {
        let fun_id = self.current_fun_id;
        self.rir.funs.get_mut(&fun_id).unwrap()
    }

    fn get_symbol(&self, exp_id: ExpId) -> Option<SymbolRef<'_>> {
        self.sema().exp_as_symbol(exp_id)
    }

    fn get_loop(&self, exp_id: ExpId) -> Option<&GenLoopDef> {
        let loop_id = self.sema().exp_as_loop(exp_id)?;
        self.rir.loops.get(&loop_id)
    }

    fn is_coerced_to_val(&self, exp_id: ExpId) -> bool {
        self.sema().exp_is_coerced_to_value(exp_id)
    }

    fn get_ty(&self, exp_id: ExpId) -> Ty {
        self.sema().exp_ty(exp_id)
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

    fn add_cmd_hi32(&mut self, target: RegId) {
        let offset = self.add_reg_imm(32);
        self.push(Cmd::BitShiftR, target, CmdArg::Reg(offset));
        self.kill(offset);
    }

    fn add_cmd_make_slice(&mut self, l: RegId, r: RegId) -> RegId {
        let offset = self.add_reg_imm(32);
        self.push(Cmd::BitShiftL, r, CmdArg::Reg(offset));
        self.push(Cmd::BitOr, l, CmdArg::Reg(r));
        self.kill(offset);
        self.kill(r);
        l
    }

    fn add_cmd_load(&mut self, dest: RegId, src: RegId, size: usize) {
        let cmd = match size {
            1 => Cmd::Load8,
            8 => Cmd::Load,
            _ => panic!("load of unsized value"),
        };
        self.push(cmd, dest, CmdArg::Reg(src));
        self.kill(src);
    }

    fn add_cmd_call_fun(&mut self, args: &[ExpId], fun_id: FunId) -> RegId {
        let body_label_id = self.rir.funs[&fun_id].body_label_id;

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
                    VarKind::Fun(..) => panic!("cannot generate fun"),
                    VarKind::Rec(_) => unreachable!("VarKind::Rec is resolved during analysis"),
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
        }
    }

    fn on_call(&mut self, exp_id: ExpId, callee: ExpId, args: &[ExpId]) -> RegId {
        let symbol_ref = self.get_symbol(callee).expect("cannot call non-symbol");
        match symbol_ref {
            SymbolRef::Prim(Prim::ByteToInt) | SymbolRef::Prim(Prim::IntToByte) => {
                self.on_exp(args[0])
            }
            SymbolRef::Prim(Prim::SliceLen) => {
                // FIXME: divide by size of inner type

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
                let scale = (self.get_ty(exp_id).as_slice_inner())
                    .expect("mem_alloc's result must be slice type")
                    .size_of()
                    .unwrap_or(1) as i64;

                let size_reg_id = self.on_exp(args[0]);
                let scale_reg_id = self.add_reg_imm(scale);
                let ptr_reg_id = self.add_reg();

                self.push(Cmd::Mul, size_reg_id, CmdArg::Reg(scale_reg_id));
                self.push(Cmd::Alloc, ptr_reg_id, CmdArg::Reg(size_reg_id));
                self.kill(size_reg_id);
                self.kill(scale_reg_id);
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
            SymbolRef::Var(
                _,
                &VarDef {
                    kind: VarKind::Fun(fun_id),
                    ..
                },
            ) => self.add_cmd_call_fun(args, fun_id),
            SymbolRef::Var(..) => panic!("cannot call on non-primitive/function value"),
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
            Op::LogOr | Op::BitOr => unimplemented!(),
            Op::Range => panic!("Can't generate range operation"),
            _ => unimplemented!(),
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
            &ExpKind::Unit => NO_REG_ID,
            &ExpKind::Int(value) => {
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Int(value));
                reg_id
            }
            &ExpKind::Byte(value) => {
                let reg_id = self.add_reg();
                self.push(Cmd::Imm, reg_id, CmdArg::Int(value as i64));
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

                let scale = (self.get_ty(indexee).as_slice_inner())
                    .expect("indexee must be slice type")
                    .size_of()
                    .unwrap_or(1);
                let scale_reg_id = self.add_reg_imm(scale as i64);

                if let Some((l, r)) = self.sema().exp_as_range(arg) {
                    // x[l..r] ---> slice(begin(x) + l * scale, begin(x) + r * scale)
                    let l_reg_id = self.on_exp(l);
                    let r_reg_id = self.on_exp(r);
                    self.push(Cmd::Mul, l_reg_id, CmdArg::Reg(scale_reg_id));
                    self.push(Cmd::Mul, r_reg_id, CmdArg::Reg(scale_reg_id));

                    self.add_cmd_lo32(indexee_reg_id);
                    self.push(Cmd::Add, l_reg_id, CmdArg::Reg(indexee_reg_id));
                    self.push(Cmd::Add, r_reg_id, CmdArg::Reg(indexee_reg_id));
                    let slice_reg_id = self.add_cmd_make_slice(l_reg_id, r_reg_id);
                    self.kill(indexee_reg_id);
                    self.kill(scale_reg_id);
                    return slice_reg_id;
                }

                let arg_reg_id = self.on_exp(arg);
                self.push(Cmd::Mul, arg_reg_id, CmdArg::Reg(scale_reg_id));
                self.push(Cmd::Add, arg_reg_id, CmdArg::Reg(indexee_reg_id));
                self.kill(indexee_reg_id);
                self.kill(scale_reg_id);

                if !self.is_coerced_to_val(exp_id) {
                    return arg_reg_id;
                }

                let item_reg_id = self.add_reg();
                self.add_cmd_lo32(arg_reg_id); // begin of slice
                self.add_cmd_load(item_reg_id, arg_reg_id, scale);
                self.kill(arg_reg_id);
                item_reg_id
            }
            &ExpKind::Bin { op, l, r } => self.on_bin(exp_id, op, l, r),
            ExpKind::Fun { .. } => NO_REG_ID,
            &ExpKind::Return(result) => {
                let end_label_id = self.current_gen_fun().end_label_id;
                let reg_id = self.on_exp(result);
                self.push(Cmd::Mov, RET_REG_ID, CmdArg::Reg(reg_id));
                self.push(Cmd::Jump, NO_REG_ID, CmdArg::Label(end_label_id));
                self.kill(reg_id);
                NO_REG_ID
            }
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
                let loop_def = self.get_loop(exp_id).expect("Missing loop def");

                let break_label = CmdArg::Label(loop_def.break_label_id);
                let continue_label = CmdArg::Label(loop_def.continue_label_id);

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
            &ExpKind::Break => {
                let loop_def = self.get_loop(exp_id).expect("Missing loop def");
                let break_label = CmdArg::Label(loop_def.break_label_id);

                self.push(Cmd::Jump, NO_REG_ID, break_label);
                NO_REG_ID
            }
            &ExpKind::Continue => {
                let loop_def = self.get_loop(exp_id).expect("Missing loop def");
                let continue_label = CmdArg::Label(loop_def.continue_label_id);

                self.push(Cmd::Jump, NO_REG_ID, continue_label);
                NO_REG_ID
            }
            &ExpKind::Let { pat, init, .. } => {
                if self.sema().exp_is_decl(exp_id) {
                    return NO_REG_ID;
                }
                let init_reg_id = self.on_exp(init);
                let var_reg_id = self.on_exp(pat);
                self.push(Cmd::Store, var_reg_id, CmdArg::Reg(init_reg_id));
                self.kill(var_reg_id);
                self.kill(init_reg_id);
                NO_REG_ID
            }
            ExpKind::Semi(exps) => self.on_exps(exps),
        }
    }

    fn on_exps(&mut self, exps: &[ExpId]) -> RegId {
        let mut reg_id = NO_REG_ID;
        for &exp_id in exps {
            let result = self.on_exp(exp_id);
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

        let final_reg_id = self.on_exps(&self.rir.sema.funs[&fun_id].bodies().to_owned());
        self.push(Cmd::Mov, RET_REG_ID, CmdArg::Reg(final_reg_id));
        self.kill(final_reg_id);

        self.add_cmd_fun_end(fun_id);
    }

    pub fn compile(&mut self) -> CompilationResult {
        // Allocate well-known registers.
        self.rir.reg_count += KNOWN_REG_NUM;

        // Prepare funs.
        let fun_ids = self.sema().funs.keys().cloned().collect::<Vec<_>>();
        for &fun_id in &fun_ids {
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
        }

        // Prepare loops.
        let loop_ids = self.sema().loops.keys().cloned().collect::<Vec<_>>();
        for &loop_id in &loop_ids {
            let break_label_id = self.add_label();
            let continue_label_id = self.add_label();
            (self.rir.loops).insert(
                loop_id,
                GenLoopDef {
                    break_label_id,
                    continue_label_id,
                },
            );
        }

        // Generate funs.
        for &fun_id in &fun_ids {
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

        // Emit compile errors.
        let (success, msgs) = Msg::summarize(self.rir.msgs.values(), &self.rir.sema.syntax);

        CompilationResult {
            success,
            program,
            msgs,
        }
    }
}

impl ShareSyntax for GenRir {
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
            CmdArg::Reg(value) => usize::from(value) as i64,
            CmdArg::Label(value) => usize::from(value) as i64,
        }
    }
}

fn lo32(x: usize) -> usize {
    x & 0xFFFF_FFFF
}

pub(crate) fn gen(sema: Rc<Sema>) -> CompilationResult {
    let mut compiler = GenRir {
        rir: Rir {
            sema: Rc::clone(&sema),
            reg_count: 0,
            label_count: 0,
            text: vec![],
            funs: BTreeMap::new(),
            loops: BTreeMap::new(),
            msgs: sema.msgs.clone(),
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    compiler.compile()
}

pub(crate) fn compile(src: &str) -> CompilationResult {
    let sema = Rc::new(analyze::analyze_str(src));
    if !sema.is_successful() {
        let (success, msgs) = Msg::summarize(sema.msgs.values(), &sema.syntax);
        return CompilationResult {
            success,
            msgs,
            program: "".to_string(),
        };
    }

    gen::gen(sema)
}
