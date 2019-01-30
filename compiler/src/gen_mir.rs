use crate::*;
use std::fmt::Write;

#[derive(Clone, Default)]
pub struct Compiler {
    pub toks: Toks,
    pub syns: Vec<Syn>,
    pub p: Mir,
    pub cur_fun_id: FunId,
}

impl Compiler {
    fn new_reg(&mut self) -> RegId {
        self.p.reg_num += 1;
        self.p.reg_num - 1
    }

    fn new_lab(&mut self) -> LabId {
        self.p.lab_num += 1;
        self.p.lab_num - 1
    }

    fn kill(&mut self, reg_id: RegId) {
        self.push(Op::Kill, reg_id, Val::None);
    }

    fn push(&mut self, op: Op, l: RegId, r: Val) -> RegId {
        self.p.funs[self.cur_fun_id].ins.push((op, l, r));
        l
    }

    fn push_str(&mut self, value: String) -> usize {
        self.p.strs.push(value.to_owned());
        self.p.strs.len() - 1
    }

    fn add_name(&mut self, name: String, var_id: VarId) {
        let fun = &mut self.p.funs[self.cur_fun_id];
        fun.vars.insert(name, var_id);
    }

    fn add_local_var(&mut self, name: String) {
        let fun = &mut self.p.funs[self.cur_fun_id];
        fun.var_num += 1;
        let var_id = (fun.var_num - 1) as i64;
        self.add_name(name, var_id);
    }

    fn find_var_id(&self, name: &str) -> Option<(VarId, bool)> {
        for &fun_id in &[self.cur_fun_id, GLOBAL_FUN_ID] {
            if let Some(&var_id) = self.p.funs[fun_id].vars.get(name) {
                return Some((var_id, fun_id != GLOBAL_FUN_ID));
            }
        }
        None
    }

    fn on_var(&mut self, name: &str) -> Option<RegId> {
        if let Some((var_id, local)) = self.find_var_id(&name) {
            let r = self.new_reg();
            if local {
                self.push(Op::Mov, r, Val::Reg(BASE_PTR_REG_ID));
                self.push(Op::AddImm, r, Val::Int(var_id));
            } else {
                self.push(Op::Imm, r, Val::Int(var_id));
            };
            Some(r)
        } else {
            None
        }
    }

    fn on_tok(&mut self, tok_id: TokId) -> RegId {
        match self.toks[tok_id].0.clone() {
            Tok::Err(err) => panic!("{}", err),
            Tok::Id(name) => {
                let l = self.new_reg();
                if let Some(r) = self.on_var(&name) {
                    self.push(Op::Load, l, Val::Reg(r));
                    self.kill(r);
                    l
                } else if name == "true" {
                    self.push(Op::Imm, l, Val::Int(1))
                } else if name == "false" {
                    self.push(Op::Imm, l, Val::Int(0))
                } else {
                    panic!("var undefined {}", name);
                }
            }
            Tok::Int(value) => {
                let l = self.new_reg();
                self.push(Op::Imm, l, Val::Int(value))
            }
            Tok::Str(value) => {
                let str_id = self.push_str(value);
                let l = self.new_reg();
                self.push(Op::Imm, l, Val::Str(str_id))
            }
            Tok::Pun(_) | Tok::Eof => unreachable!(),
        }
    }

    fn to_str(&self, syn_id: SynId) -> &str {
        if let &Syn::Val(tok_id) = &self.syns[syn_id] {
            match &self.toks[tok_id].0 {
                &Tok::Id(ref id) => id,
                &Tok::Str(ref value) => value,
                tok => panic!("{:?} must be str or id", tok),
            }
        } else {
            panic!("{} must be an str or id", syn_id)
        }
    }

    fn do_app(&mut self, name: &str, syns: &[SynId]) -> RegId {
        for fun in &self.p.funs {
            if name == fun.name {
                let fun_lab = fun.lab_id;

                // Push args to stack.
                for i in 0..syns.len() {
                    let r = self.on_exp(syns[i]);
                    self.push(Op::Push, r, Val::None);
                    self.kill(r);
                }

                self.push(Op::Call, NO_REG_ID, Val::Lab(fun_lab));

                // Pop args from stack.
                let l = self.new_reg();
                for _ in 0..syns.len() {
                    self.push(Op::Pop, l, Val::None);
                }
                self.kill(l);

                // The result value must be stored in the reg.
                return RET_REG_ID;
            }
        }

        if let Some(bin_id) = BINS.iter().position(|&(x, _)| x == name) {
            let l = self.on_exp(syns[0]);
            for i in 1..syns.len() {
                let r = self.on_exp(syns[i]);
                self.push(BINS[bin_id].1, l, Val::Reg(r));
                self.kill(r);
            }
            l
        } else if let Some(id) = RELS.iter().position(|&(x, _)| x == name) {
            let l = self.on_exp(syns[0]);
            let r = self.on_exp(syns[1]);
            self.push(RELS[id].1, l, Val::Reg(r));
            self.kill(r);
            l
        } else if name == "let" {
            let name = self.to_str(syns[0]).to_owned();
            // Evaluate default value.
            let l = self.on_exp(syns[1]);
            // Create variable.
            self.add_local_var(name.clone());
            // Set default value.
            let r = self.on_var(&name).unwrap();
            self.push(Op::Store, l, Val::Reg(r));
            self.kill(l);
            self.kill(r);
            NO_REG_ID
        } else if name == "def" {
            let arg_syns = match self.syns[syns[0]].clone() {
                Syn::App(arg_syns) => arg_syns,
                _ => panic!("first arg of def must be app"),
            };
            let name = self.to_str(arg_syns[0]).to_owned();

            // Save current context.
            let cur_fun_id = self.cur_fun_id;

            // Start function context.
            {
                let lab_id = self.new_lab();
                self.p.funs.push(Fun {
                    name: name,
                    lab_id: lab_id,
                    ..Fun::default()
                });
                self.cur_fun_id = self.p.funs.len() - 1;

                // Define parameters as local variables.
                for i in 1..arg_syns.len() {
                    let name = self.to_str(arg_syns[i]).to_owned();
                    let var_id = i as i64 - arg_syns.len() as i64;
                    self.add_name(name, var_id);
                }

                self.push(Op::Label, 0, Val::Lab(lab_id));
                let l = self.on_exp(syns[1]);
                self.push(Op::Mov, RET_REG_ID, Val::Reg(l));
                self.kill(l);
                self.push(Op::Ret, NO_REG_ID, Val::None);
            }

            // Restore context.
            self.cur_fun_id = cur_fun_id;
            NO_REG_ID
        } else if name == "begin" {
            let mut l = NO_REG_ID;
            for i in 0..syns.len() {
                let r = self.on_exp(syns[i]);
                self.kill(l);
                l = r;
            }
            l
        } else if name == "cond" {
            // Label to point to the end of cond.
            let end_lab = Val::Lab(self.new_lab());
            // Register to set the result of cond.
            let end_reg = self.new_reg();
            let mut i = 0;
            while i < syns.len() {
                if i + 1 < syns.len() {
                    // .. cond then_cl ..
                    let else_lab = Val::Lab(self.new_lab());
                    let cond = self.on_exp(syns[i]);
                    // Unless cond is true, go to next clause.
                    self.push(Op::Unless, cond, else_lab);
                    self.kill(cond);
                    // Evaluate the then clause.
                    let then_cl = self.on_exp(syns[i + 1]);
                    // Set the result to the result register.
                    self.push(Op::Mov, end_reg, Val::Reg(then_cl));
                    self.kill(then_cl);
                    // Jump to the end of cond.
                    self.push(Op::Jump, NO_REG_ID, end_lab);
                    // Come from the `unless`.
                    self.push(Op::Label, NO_REG_ID, else_lab);
                    i += 2;
                } else {
                    // .. else_cl
                    let else_cl = self.on_exp(syns[i]);
                    self.push(Op::Mov, end_reg, Val::Reg(else_cl));
                    self.kill(else_cl);
                    i += 1;
                }
            }
            self.push(Op::Label, NO_REG_ID, end_lab);
            end_reg
        } else if name == "while" {
            let beg_lab = Val::Lab(self.new_lab());
            let end_lab = Val::Lab(self.new_lab());
            self.push(Op::Label, NO_REG_ID, beg_lab);
            let cond = self.on_exp(syns[0]);
            // Unless cond is true, go to end.
            self.push(Op::Unless, cond, end_lab);
            self.kill(cond);
            // Evaluate the body.
            let l = self.on_exp(syns[1]);
            self.kill(l);
            // Jump to the begin label to continue.
            self.push(Op::Jump, NO_REG_ID, beg_lab);
            // Come on break.
            self.push(Op::Label, NO_REG_ID, end_lab);
            NO_REG_ID
        } else if name == "set" {
            let name = self.to_str(syns[0]).to_owned();
            let l = self.on_exp(syns[1]);
            let r = self.on_var(&name).expect("unknown var");
            self.push(Op::Store, l, Val::Reg(r));
            self.kill(l);
            self.kill(r);
            NO_REG_ID
        } else if name == "to_str" {
            let l = self.on_exp(syns[0]);
            self.push(Op::ToStr, l, Val::None)
        } else if name == "read_int" {
            let l = self.new_reg();
            self.push(Op::ReadInt, l, Val::None)
        } else if name == "read_str" {
            let l = self.new_reg();
            self.push(Op::ReadStr, l, Val::None)
        } else if name == "print" {
            for i in 0..syns.len() {
                let l = self.on_exp(syns[i]);
                self.push(Op::Print, l, Val::None);
                self.kill(l);
            }
            NO_REG_ID
        } else if name == "println" {
            for i in 0..syns.len() {
                let last = i + 1 == syns.len();
                let l = self.on_exp(syns[i]);
                self.push(if last { Op::PrintLn } else { Op::Print }, l, Val::None);
                self.kill(l);
            }
            NO_REG_ID
        } else {
            panic!("unknown callee {}", name)
        }
    }

    fn on_exp(&mut self, syn_id: SynId) -> RegId {
        let syn = self.syns[syn_id].clone();
        match syn {
            Syn::Err(err, _) => panic!("{}", err),
            Syn::Val(tok_id) => self.on_tok(tok_id),
            Syn::App(syns) => match self.syns[syns[0]].clone() {
                Syn::Val(tok_id) => match self.toks[tok_id].0.clone() {
                    Tok::Id(head) => self.do_app(&head, &syns[1..]),
                    tok => panic!("{:?} callee must be identifier", &tok),
                },
                syn => panic!("callee must be identifier {:?}", syn),
            },
        }
    }

    fn gen(&mut self) {
        // Allocate well-known registers.
        self.p.reg_num += KNOWN_REG_NUM;

        self.p.funs.push(Fun::default()); // Entry function.

        let entry_syn_id = self.syns.len() - 1;

        let exit = self.on_exp(entry_syn_id);
        self.kill(exit);

        self.push(Op::Exit, NO_REG_ID, Val::None);
    }

    pub fn compile(mut self) -> String {
        self.gen();

        regalloc::alloc_regs(&mut self.p);

        // Merge instructions.
        let mut ins = vec![];
        for fun in self.p.funs.iter_mut() {
            ins.append(&mut fun.ins);
        }

        // Build jump table.
        let mut labels = vec![0; self.p.lab_num];
        for pc in 0..ins.len() {
            if let (Op::Label, _, Val::Lab(r)) = ins[pc] {
                labels[r] = pc;
            }
        }

        // Replace label ids with pc.
        for i in 0..ins.len() {
            if let Val::Lab(lab_id) = ins[i].2 {
                ins[i].2 = Val::Int(labels[lab_id] as i64);
            }
        }

        // Write MIR.
        let mut buffer = String::new();
        for value in self.p.strs {
            writeln!(buffer, "    .text {}", value).unwrap();
        }
        for (op, l, r) in ins {
            let op = crate::serialize_op(op);
            writeln!(buffer, "    {} {} {}", op, l, r.to_i64()).unwrap();
        }

        buffer
    }
}

impl Val {
    fn to_i64(self) -> i64 {
        match self {
            Val::None => 0,
            Val::Int(value) => value,
            Val::Reg(value) | Val::Lab(value) | Val::Str(value) => value as i64,
        }
    }
}
