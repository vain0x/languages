use crate::sema::Prim;
use crate::sema::Sema;
use crate::*;
use std::fmt::Write;
use std::mem::size_of;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    None,
    Int(i64),
    Str(StrId),
    Reg(RegId),
    Label(LabelId),
}

pub type Ins = (Op, RegId, Value);

#[derive(Clone, Default)]
pub struct FunDef {
    label_id: LabelId,
    ins: Vec<Ins>,
}

pub struct Mir {
    pub sema: Rc<Sema>,
    pub strs: Vec<String>,
    pub funs: Vec<FunDef>,
    pub reg_count: usize,
    pub label_count: usize,
    pub msgs: Vec<Msg>,
}

struct Compiler {
    mir: Mir,
    current_fun_id: FunId,
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

impl Mir {
    fn add_reg(&mut self) -> RegId {
        self.reg_count += 1;
        self.reg_count - 1
    }

    fn add_label(&mut self) -> LabelId {
        self.label_count += 1;
        self.label_count - 1
    }
}

impl Compiler {
    fn exp_children(&self, exp_id: ExpId) -> &[ExpId] {
        &self.mir.sema.exps[exp_id].children
    }

    fn add_reg(&mut self) -> RegId {
        self.mir.add_reg()
    }

    fn add_label(&mut self) -> LabelId {
        self.mir.add_label()
    }

    fn kill(&mut self, reg_id: RegId) {
        self.push(Op::Kill, reg_id, Value::None);
    }

    fn push(&mut self, op: Op, l: RegId, r: Value) -> RegId {
        self.mir.funs[self.current_fun_id].ins.push((op, l, r));
        l
    }

    fn add_str(&mut self, value: String) -> usize {
        self.mir.strs.push(value.to_owned());
        self.mir.strs.len() - 1
    }

    fn add_err(&mut self, message: String, node_id: NodeId) -> RegId {
        self.mir.msgs.push(Msg::err(message, node_id));
        self.push(Op::Exit, NO_REG_ID, Value::None)
    }

    fn on_var(&mut self, var_id: VarId) -> RegId {
        let r = self.add_reg();
        let sema::VarDef { kind, index, .. } = &self.mir.sema.vars[var_id];
        let index = *index as i64;
        let unit = size_of::<i64>() as i64;
        match kind {
            sema::VarKind::Global | sema::VarKind::Local => {
                let offset = -(index + 1) * unit;
                // i-th local variable is located at (bp + i)
                self.push(Op::Mov, r, Value::Reg(BASE_PTR_REG_ID));
                self.push(Op::AddImm, r, Value::Int(offset));
            }
            &sema::VarKind::Param(fun_id) => {
                // i-th argument is located before bp in reversed order.
                let arity = self.mir.sema.funs[fun_id].arity() as i64;
                let offset = (arity - index - 1) * unit;
                self.push(Op::Mov, r, Value::Reg(BASE_PTR_REG_ID));
                self.push(Op::AddImm, r, Value::Int(offset));
            }
        };
        r
    }

    fn on_exp_call(&mut self, fun_id: FunId, exp_id: ExpId) -> RegId {
        let children = self.exp_children(exp_id).to_owned();
        let fun = &self.mir.funs[fun_id];
        let fun_label = fun.label_id;

        // Push args to stack.
        for i in 0..children.len() {
            let r = self.on_exp(children[i]);
            self.push(Op::Push, r, Value::None);
            self.kill(r);
        }

        self.push(Op::Call, NO_REG_ID, Value::Label(fun_label));

        // Pop args from stack.
        let l = self.add_reg();
        for _ in 0..children.len() {
            self.push(Op::Pop, l, Value::None);
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
                self.push(PRIM_OP_2[i].1, l, Value::Reg(r));
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
                let end_label = Value::Label(self.add_label());
                let end_reg = self.add_reg();
                let mut i = 0;
                while i < children.len() {
                    if i + 1 < children.len() {
                        // .. cond then_cl ..
                        let else_label = Value::Label(self.add_label());
                        let cond = self.on_exp(children[i]);
                        // Unless cond is true, go to next clause.
                        self.push(Op::Unless, cond, else_label);
                        self.kill(cond);
                        // Evaluate the then clause.
                        let then_clause = self.on_exp(children[i + 1]);
                        // Set the result to the result register.
                        self.push(Op::Mov, end_reg, Value::Reg(then_clause));
                        self.kill(then_clause);
                        // Jump to the end of cond.
                        self.push(Op::Jump, NO_REG_ID, end_label);
                        // Come from the `unless`.
                        self.push(Op::Label, NO_REG_ID, else_label);
                        i += 2;
                    } else {
                        // .. else_lase
                        let else_clause = self.on_exp(children[i]);
                        self.push(Op::Mov, end_reg, Value::Reg(else_clause));
                        self.kill(else_clause);
                        i += 1;
                    }
                }
                self.push(Op::Label, NO_REG_ID, end_label);
                end_reg
            }
            Prim::While => {
                let beg_label = Value::Label(self.add_label());
                let end_label = Value::Label(self.add_label());
                self.push(Op::Label, NO_REG_ID, beg_label);
                let cond = self.on_exp(children[0]);
                // Unless cond is true, go to end.
                self.push(Op::Unless, cond, end_label);
                self.kill(cond);
                // Evaluate the body.
                let l = self.on_exp(children[1]);
                self.kill(l);
                // Jump to the begin label to continue.
                self.push(Op::Jump, NO_REG_ID, beg_label);
                // Come on break.
                self.push(Op::Label, NO_REG_ID, end_label);
                NO_REG_ID
            }
            Prim::ToStr => {
                let l = self.on_exp(children[0]);
                self.push(Op::ToStr, l, Value::None)
            }
            Prim::ReadInt | Prim::ReadStr => {
                let op = if prim == Prim::ReadInt {
                    Op::ReadInt
                } else {
                    Op::ReadStr
                };
                let l = self.add_reg();
                self.push(op, l, Value::None)
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
                    self.push(op, l, Value::None);
                    self.kill(l);
                }
                NO_REG_ID
            }
            Prim::MemAlloc => {
                let r = self.on_exp(children[0]); // size
                let l = self.add_reg();
                self.push(Op::Alloc, l, Value::Reg(r));
                self.kill(r);
                l
            }
            Prim::WriteByte => {
                let l = self.on_exp(children[0]); // ptr
                let r = self.on_exp(children[1]); // value
                self.push(Op::Store8, l, Value::Reg(r));
                self.kill(l);
                self.kill(r);
                NO_REG_ID
            }
            Prim::StdOutWrite => {
                let l = self.on_exp(children[0]); // ptr
                let r = self.on_exp(children[1]); // size
                self.push(Op::Write, l, Value::Reg(r));
                self.kill(l);
                self.kill(r);
                NO_REG_ID
            }
            _ => unreachable!("{:?}", prim),
        }
    }

    fn on_exp(&mut self, exp_id: ExpId) -> RegId {
        match &self.mir.sema.exps[exp_id].kind {
            sema::ExpKind::Err(err, node_id) => self.add_err(err.to_owned(), *node_id),
            sema::ExpKind::None => NO_REG_ID,
            &sema::ExpKind::Int(value) => {
                let l = self.add_reg();
                self.push(Op::Imm, l, Value::Int(value))
            }
            sema::ExpKind::Str(value) => {
                let str_id = self.add_str(value.to_owned());
                let l = self.add_reg();
                self.push(Op::Imm, l, Value::Str(str_id))
            }
            &sema::ExpKind::Var(var_id) => {
                let l = self.add_reg();
                let r = self.on_var(var_id);
                self.push(Op::Load, l, Value::Reg(r));
                self.kill(r);
                l
            }
            &sema::ExpKind::App(sema::FunRef::Fun(fun_id)) => self.on_exp_call(fun_id, exp_id),
            &sema::ExpKind::App(sema::FunRef::Prim(prim)) => self.on_exp_prim(prim, exp_id),
            &sema::ExpKind::Set(var_id) => {
                let r = self.on_var(var_id);
                let l = self.on_exp(self.exp_children(exp_id)[0]);
                // Store the value of l to the position the register r refers to. It's absurd.
                self.push(Op::Store, l, Value::Reg(r));
                self.kill(r);
                l
            }
        }
    }

    pub fn compile(&mut self) -> (bool, String, String) {
        // Allocate well-known registers.
        self.mir.reg_count += KNOWN_REG_NUM;

        // Create function labels.
        for _ in 0..self.mir.sema.funs.len() {
            let label_id = self.add_label();
            let fun = FunDef {
                label_id,
                ins: vec![],
            };
            self.mir.funs.push(fun);
        }
        assert_eq!(self.mir.funs.len(), self.mir.sema.funs.len());

        // Generate codes for each function.
        for fun_id in 0..self.mir.funs.len() {
            self.current_fun_id = fun_id;

            self.push(Op::Label, 0, Value::Label(self.mir.funs[fun_id].label_id));

            let last = self.on_exp(self.mir.sema.funs[fun_id].body);

            if fun_id == GLOBAL_FUN_ID {
                self.kill(last);
                self.push(Op::Exit, NO_REG_ID, Value::None);
            } else {
                self.push(Op::Mov, RET_REG_ID, Value::Reg(last));
                self.kill(last);
                self.push(Op::Ret, NO_REG_ID, Value::None);
            }
        }

        // Reassign registers to finite number registers.
        for fun_id in 0..self.mir.funs.len() {
            regalloc::alloc_regs(&mut self.mir.funs[fun_id].ins);
        }

        // Merge instructions.
        let mut ins = vec![];
        for fun in self.mir.funs.iter_mut() {
            ins.append(&mut fun.ins);
        }

        // Build jump table.
        let mut labels = vec![0; self.mir.label_count];
        for pc in 0..ins.len() {
            if let (Op::Label, _, Value::Label(r)) = ins[pc] {
                labels[r] = pc;
            }
        }

        // Replace label ids with pc.
        for i in 0..ins.len() {
            if let Value::Label(label_id) = ins[i].2 {
                ins[i].2 = Value::Int(labels[label_id] as i64);
            }
        }

        // Write MIR.
        let mut buffer = String::new();
        for value in &self.mir.strs {
            writeln!(buffer, "    .text {}", value).unwrap();
        }
        for (op, l, r) in ins {
            let op = crate::serialize_op(op);
            writeln!(buffer, "    {} {} {}", op, l, r.to_i64()).unwrap();
        }

        // Emit compile errors.
        let mut success = true;
        let mut errors = String::new();
        for msg in &self.mir.msgs {
            let ((ly, lx), (ry, rx)) = self.mir.sema.syntax.locate_node(msg.node_id);
            writeln!(
                errors,
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

        (success, buffer, errors)
    }
}

impl Value {
    fn to_i64(self) -> i64 {
        match self {
            Value::None => 0,
            Value::Int(value) => value,
            Value::Reg(value) | Value::Label(value) | Value::Str(value) => value as i64,
        }
    }
}

pub fn gen_mir(sema: Rc<Sema>) -> (bool, String, String) {
    let mut compiler = Compiler {
        mir: Mir {
            sema,
            funs: vec![],
            strs: vec![],
            reg_count: 0,
            label_count: 0,
            msgs: vec![],
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    compiler.compile()
}
