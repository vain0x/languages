//! Evaluate picomet-lang code.

use crate::mir::gen_mir::compile;
use crate::semantics::msg::DocMsg;
use std::io::{self, Read};

pub fn run() {
    let mut stdin = io::stdin();
    let mut picomet_code = vec![];
    stdin.read_to_end(&mut picomet_code).unwrap();
    let picomet_code = String::from_utf8(picomet_code).unwrap();

    let result = compile(&picomet_code);
    if !result.success {
        eprintln!("{}", DocMsg::to_text(&result.msgs));
        std::process::exit(1)
    }

    picomet_lang_runtime::eval_with_stdio(&result.program);
}
