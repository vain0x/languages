use crate::*;

struct RegAlloc {
    used: Vec<bool>,
    reg_map: BTreeMap<RegId, RegId>,
}

impl RegAlloc {
    fn alloc(&mut self, reg_id: RegId) -> RegId {
        if reg_id == NO_REG_ID {
            return RET_REG_ID;
        }
        if let Some(&reg_id) = self.reg_map.get(&reg_id) {
            return reg_id;
        }
        for i in 0..REG_NUM {
            if !self.used[i] {
                self.reg_map.insert(reg_id, i);
                self.used[i] = true;
                return i;
            }
        }
        panic!("too many registers are required")
    }

    fn kill(&mut self, reg_id: RegId) -> RegId {
        let reg_id = self.alloc(reg_id);
        self.used[reg_id] = false;
        reg_id
    }

    /// Verify all registers are killed.
    fn verify(&mut self) {
        for reg_id in KNOWN_REG_NUM..REG_NUM {
            assert!(!self.used[reg_id]);
        }
    }
}

fn alloc_regs_fun(fun: &mut Fun) {
    let mut reg_alloc = RegAlloc {
        used: vec![false; REG_NUM],
        reg_map: BTreeMap::new(),
    };

    // Allocate known regs.
    // These register numbers must not change.
    for reg_id in 0..KNOWN_REG_NUM {
        let dest_reg_id = reg_alloc.alloc(reg_id);
        assert_eq!(dest_reg_id, reg_id);
    }

    for ins in &mut fun.ins {
        if let Op::Kill = ins.0 {
            ins.1 = reg_alloc.kill(ins.1);
            continue;
        }

        ins.1 = reg_alloc.alloc(ins.1);
        if let Val::Reg(ref mut reg_id) = ins.2 {
            *reg_id = reg_alloc.alloc(*reg_id)
        }
    }

    reg_alloc.verify();

    // Remove NOP instructions.
    fun.ins.retain(|ins| ins.0 != Op::Kill);
}

pub fn alloc_regs(p: &mut Mir) {
    for fun in &mut p.funs {
        alloc_regs_fun(fun);
    }
}
