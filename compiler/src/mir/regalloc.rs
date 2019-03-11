use crate::*;

struct RegAlloc {
    used: Vec<bool>,
    reg_map: BTreeMap<RegId, RegId>,
}

impl RegAlloc {
    fn alloc(&mut self, reg_id: RegId) -> RegId {
        if reg_id < RegId::new(KNOWN_REG_NUM) {
            return reg_id;
        }
        if let Some(&reg_id) = self.reg_map.get(&reg_id) {
            return reg_id;
        }
        for i in KNOWN_REG_NUM..REG_NUM {
            if !self.used[i] {
                self.reg_map.insert(reg_id, RegId::new(i));
                self.used[i] = true;
                return RegId::new(i);
            }
        }
        panic!("too many registers are required")
    }

    fn kill(&mut self, reg_id: RegId) -> RegId {
        if reg_id < RegId::new(KNOWN_REG_NUM) {
            return reg_id;
        }
        let reg_id = self.alloc(reg_id);
        self.used[reg_id.0] = false;
        reg_id
    }

    /// Verify all registers are killed.
    fn verify(&mut self) {
        for reg_id in KNOWN_REG_NUM..REG_NUM {
            assert!(!self.used[reg_id], "reg_id = {} not killed", reg_id);
        }
    }
}

pub(crate) fn alloc_regs(inss: &mut Vec<Ins>) {
    let mut reg_alloc = RegAlloc {
        used: vec![false; REG_NUM],
        reg_map: BTreeMap::new(),
    };

    for ins in inss.iter_mut() {
        if let Cmd::Kill = ins.0 {
            ins.1 = reg_alloc.kill(ins.1);
            continue;
        }

        ins.1 = reg_alloc.alloc(ins.1);
        if let CmdArg::Reg(ref mut reg_id) = ins.2 {
            *reg_id = reg_alloc.alloc(*reg_id)
        }
    }

    reg_alloc.verify();

    // Remove NOP instructions.
    inss.retain(|ins| ins.0 != Cmd::Kill);
}
