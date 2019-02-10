use crate::sema::Prim;
use crate::*;
use std::fmt::Write;

#[derive(Clone, Copy, Debug)]
pub enum Val {
    None,
    Int(i64),
    Str(StrId),
    Reg(RegId),
    Lab(LabId),
}

pub type Ins = (Op, RegId, Val);

#[derive(Clone, Default)]
pub struct FunDef {
    lab_id: LabId,
    ins: Vec<Ins>,
}

#[derive(Clone, Default)]
pub struct Compiler {
    pub toks: Vec<Tok>,
    pub syns: Vec<Syn>,
    pub sema_vars: Vec<sema::VarDef>,
    pub sema_funs: Vec<sema::FunDef>,
    pub sema_exps: Vec<sema::Exp>,
    pub strs: Vec<String>,
    pub funs: Vec<FunDef>,
    pub cur_fun_id: FunId,
    pub reg_num: usize,
    pub lab_num: usize,
}

const PRIM_OP_2: &[(Prim, Op)] = &[
    (Prim::AddInt, Op::Add),
    (Prim::SubInt, Op::Sub),
    (Prim::Mul, Op::Mul),
    (Prim::Div, Op::Div),
    (Prim::Mod, Op::Mod),
    (Prim::Eq, Op::Eq),
    (Prim::Ne, Op::Ne),
    (Prim::Lt, Op::Lt),
    (Prim::Le, Op::Le),
    (Prim::Gt, Op::Gt),
    (Prim::Ge, Op::Ge),
    (Prim::AddStr, Op::StrCat),
];

impl Compiler {
    fn exp_children(&self, exp_id: ExpId) -> &[ExpId] {
        &self.sema_exps[exp_id].children
    }

    fn new_reg(&mut self) -> RegId {
        self.reg_num += 1;
        self.reg_num - 1
    }

    fn new_lab(&mut self) -> LabId {
        self.lab_num += 1;
        self.lab_num - 1
    }

    fn kill(&mut self, reg_id: RegId) {
        self.push(Op::Kill, reg_id, Val::None);
    }

    fn push(&mut self, op: Op, l: RegId, r: Val) -> RegId {
        self.funs[self.cur_fun_id].ins.push((op, l, r));
        l
    }

    fn add_str(&mut self, value: String) -> usize {
        self.strs.push(value.to_owned());
        self.strs.len() - 1
    }

    fn on_var(&mut self, var_id: VarId) -> RegId {
        let r = self.new_reg();
        let sema::VarDef { kind, index, .. } = &self.sema_vars[var_id];
        let index = *index as i64;
        match kind {
            sema::VarKind::Global | sema::VarKind::Local => {
                // i-th local variable is located at (bp + i)
                self.push(Op::Mov, r, Val::Reg(BASE_PTR_REG_ID));
                self.push(Op::AddImm, r, Val::Int(index));
            }
            &sema::VarKind::Param(fun_id) => {
                // i-th argument is located before bp in reversed order.
                let arity = self.sema_funs[fun_id].arity() as i64;
                let offset = index - arity;
                self.push(Op::Mov, r, Val::Reg(BASE_PTR_REG_ID));
                self.push(Op::AddImm, r, Val::Int(offset));
            }
        };
        r
    }

    fn on_exp_call(&mut self, fun_id: FunId, exp_id: ExpId) -> RegId {
        let children = self.exp_children(exp_id).to_owned();
        let fun = &self.funs[fun_id];
        let fun_lab = fun.lab_id;

        // Push args to stack.
        for i in 0..children.len() {
            let r = self.on_exp(children[i]);
            self.push(Op::Push, r, Val::None);
            self.kill(r);
        }

        self.push(Op::Call, NO_REG_ID, Val::Lab(fun_lab));

        // Pop args from stack.
        let l = self.new_reg();
        for _ in 0..children.len() {
            self.push(Op::Pop, l, Val::None);
        }
        self.kill(l);

        // The result value must be stored in the reg.
        RET_REG_ID
    }

    fn on_exp_prim(&mut self, prim: sema::Prim, exp_id: ExpId) -> RegId {
        let children = self.exp_children(exp_id).to_owned();

        for i in 0..PRIM_OP_2.len() {
            if PRIM_OP_2[i].0 == prim {
                let l = self.on_exp(children[0]);
                let r = self.on_exp(children[1]);
                self.push(PRIM_OP_2[i].1, l, Val::Reg(r));
                self.kill(r);
                return l;
            }
        }

        match prim {
            Prim::Begin => {
                let mut l = NO_REG_ID;
                for i in 0..children.len() {
                    let r = self.on_exp(children[i]);
                    self.kill(l);
                    l = r;
                }
                l
            }
            Prim::Cond => {
                let end_lab = Val::Lab(self.new_lab());
                let end_reg = self.new_reg();
                let mut i = 0;
                while i < children.len() {
                    if i + 1 < children.len() {
                        // .. cond then_cl ..
                        let else_lab = Val::Lab(self.new_lab());
                        let cond = self.on_exp(children[i]);
                        // Unless cond is true, go to next clause.
                        self.push(Op::Unless, cond, else_lab);
                        self.kill(cond);
                        // Evaluate the then clause.
                        let then_cl = self.on_exp(children[i + 1]);
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
                        let else_cl = self.on_exp(children[i]);
                        self.push(Op::Mov, end_reg, Val::Reg(else_cl));
                        self.kill(else_cl);
                        i += 1;
                    }
                }
                self.push(Op::Label, NO_REG_ID, end_lab);
                end_reg
            }
            Prim::While => {
                let beg_lab = Val::Lab(self.new_lab());
                let end_lab = Val::Lab(self.new_lab());
                self.push(Op::Label, NO_REG_ID, beg_lab);
                let cond = self.on_exp(children[0]);
                // Unless cond is true, go to end.
                self.push(Op::Unless, cond, end_lab);
                self.kill(cond);
                // Evaluate the body.
                let l = self.on_exp(children[1]);
                self.kill(l);
                // Jump to the begin label to continue.
                self.push(Op::Jump, NO_REG_ID, beg_lab);
                // Come on break.
                self.push(Op::Label, NO_REG_ID, end_lab);
                NO_REG_ID
            }
            Prim::ToStr => {
                let l = self.on_exp(children[0]);
                self.push(Op::ToStr, l, Val::None)
            }
            Prim::ReadInt | Prim::ReadStr => {
                let op = if prim == Prim::ReadInt {
                    Op::ReadInt
                } else {
                    Op::ReadStr
                };
                let l = self.new_reg();
                self.push(op, l, Val::None)
            }
            Prim::Print | Prim::PrintLn => {
                for i in 0..children.len() {
                    let last = i + 1 == children.len();
                    let op = if last && prim == Prim::PrintLn {
                        Op::PrintLn
                    } else {
                        Op::Print
                    };
                    let l = self.on_exp(children[i]);
                    self.push(op, l, Val::None);
                    self.kill(l);
                }
                NO_REG_ID
            }
            _ => unreachable!("{:?}", prim),
        }
    }

    fn on_exp(&mut self, exp_id: ExpId) -> RegId {
        match &self.sema_exps[exp_id].kind {
            sema::ExpKind::Err(err) => panic!("{}", err),
            sema::ExpKind::None => NO_REG_ID,
            &sema::ExpKind::Int(value) => {
                let l = self.new_reg();
                self.push(Op::Imm, l, Val::Int(value))
            }
            sema::ExpKind::Str(value) => {
                let str_id = self.add_str(value.to_owned());
                let l = self.new_reg();
                self.push(Op::Imm, l, Val::Str(str_id))
            }
            &sema::ExpKind::Var(var_id) => {
                let l = self.new_reg();
                let r = self.on_var(var_id);
                self.push(Op::Load, l, Val::Reg(r));
                self.kill(r);
                l
            }
            &sema::ExpKind::App(sema::FunRef::Fun(fun_id)) => self.on_exp_call(fun_id, exp_id),
            &sema::ExpKind::App(sema::FunRef::Prim(prim)) => self.on_exp_prim(prim, exp_id),
            &sema::ExpKind::Set(var_id) => {
                let r = self.on_var(var_id);
                let l = self.on_exp(self.exp_children(exp_id)[0]);
                // Store the value of l to the position the register r refers to. It's absurd.
                self.push(Op::Store, l, Val::Reg(r));
                self.kill(r);
                l
            }
        }
    }

    pub fn compile(mut self) -> String {
        // Allocate well-known registers.
        self.reg_num += KNOWN_REG_NUM;

        // Create function labels.
        for _ in 0..self.sema_funs.len() {
            let lab_id = self.new_lab();
            let fun = FunDef {
                lab_id,
                ins: vec![],
            };
            self.funs.push(fun);
        }
        assert_eq!(self.funs.len(), self.sema_funs.len());

        // Generate codes for each function.
        for fun_id in 0..self.funs.len() {
            self.cur_fun_id = fun_id;

            self.push(Op::Label, 0, Val::Lab(self.funs[fun_id].lab_id));

            let last = self.on_exp(self.sema_funs[fun_id].body);

            if fun_id == GLOBAL_FUN_ID {
                self.kill(last);
                self.push(Op::Exit, NO_REG_ID, Val::None);
            } else {
                self.push(Op::Mov, RET_REG_ID, Val::Reg(last));
                self.kill(last);
                self.push(Op::Ret, NO_REG_ID, Val::None);
            }
        }

        // Reassign registers to finite number registers.
        for fun_id in 0..self.funs.len() {
            regalloc::alloc_regs(&mut self.funs[fun_id].ins);
        }

        // Merge instructions.
        let mut ins = vec![];
        for fun in self.funs.iter_mut() {
            ins.append(&mut fun.ins);
        }

        // Build jump table.
        let mut labels = vec![0; self.lab_num];
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
        for value in self.strs {
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
