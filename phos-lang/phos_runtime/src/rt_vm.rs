use super::*;
use std::collections::HashMap;

pub(crate) struct Vm {
    codes: &'static [&'static str],
    pc: Cell<usize>,
    m: &'static PilModData,
    regs: HashMap<PilSymbol, Val>,
}

impl Vm {
    pub(crate) fn new(m: PilModData) -> Self {
        let m = &*Box::leak(Box::new(m));
        let f = &m.fns[&as_symbol("<root>")];

        Self {
            codes: &f.codes,
            pc: Cell::new(0),
            m,
            regs: HashMap::new(),
        }
    }

    fn scan_token(&self) -> Option<&'static str> {
        let i = self.pc.get();
        self.pc.set(i + 1);

        self.codes.get(i).copied()
    }

    fn scan_reg(&self) -> PilSymbol {
        let token = self.scan_token().expect("reg");
        as_symbol(token)
    }

    fn scan_str(&self) -> String {
        let token = self.scan_token().expect("str");
        assert!(token.starts_with("\""));
        assert!(token.ends_with("\""));
        assert!(token.len() >= 2);

        token[1..token.len() - 1].replace("\\n", "\n")
    }

    fn scan_int(&self) -> i64 {
        let token = self.scan_token().expect("int");
        token
            .parse()
            .unwrap_or_else(|_| panic!("illegal int: {}", token))
    }

    fn reg_fetch(&mut self, reg: PilSymbol) -> &mut Val {
        self.regs.get_mut(&reg).unwrap()
    }

    fn execute(&mut self) {
        let token = self.scan_token().expect("reached at EOF without exit");
        match token {
            "im_str" => {
                let reg = self.scan_reg();
                let value = self.scan_str();
                self.regs.insert(reg, Val::Str(value.to_string()));
            }
            "im_int" => {
                let reg = self.scan_reg();
                let value = self.scan_int();
                self.regs.insert(reg, Val::Int(value));
            }
            "ex_add" => {
                let reg = self.scan_reg();
                let rhs = self.scan_reg();

                let rhs = self.reg_fetch(rhs).clone();
                let lhs = self.reg_fetch(reg);
                match lhs {
                    Val::Str(lhs) => *lhs += &rhs.to_str(),
                    Val::Int(lhs) => *lhs += rhs.to_int(),
                }
            }
            "ex_println" => {
                let reg = self.scan_reg();
                let value = self.reg_fetch(reg).to_str();

                if !check_stdout_ln(self.m.check.stdout.as_ref(), &value) {
                    println!("{}", value);
                }
            }
            "exit" => {
                let code = self.scan_int() as i32;

                check_exit_code(code, &self.m.check);
                std::process::exit(code);
            }
            _ => panic!("expected instr: {}", token),
        }
    }

    pub(crate) fn run(&mut self) {
        loop {
            self.execute();
        }
    }
}

fn check_stdout(cell_opt: Option<&Cell<&'static str>>, data: &str) -> bool {
    if let Some(cell) = cell_opt {
        let mut expected = cell.get();
        assert!(
            expected.starts_with(data),
            "unexpected print expected={} actual={}",
            expected,
            data
        );
        expected = &expected[data.len()..];
        cell.set(expected);
        true
    } else {
        false
    }
}

fn check_stdout_ln(cell_opt: Option<&Cell<&'static str>>, data: &str) -> bool {
    check_stdout(cell_opt, &format!("{}\n", data))
}

fn check_exit_code(code: i32, check: &PilCheck) {
    if let Some(expected) = check.exit_code {
        assert_eq!(code, expected, "illegal exit code");

        // 終了前に標準出力が最後まで終わったか検査する。
        if let Some(cell) = check.stdout.as_ref() {
            assert_eq!(
                cell.get(),
                "",
                "標準出力が不足した状態で exit に到達しました。",
            );
        }

        std::process::exit(0)
    }
}
