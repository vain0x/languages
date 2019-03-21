//! Evaluate picomet-lang code.

use crate::mir::gen_mir::compile;
use crate::semantics::msg::DocMsg;
use std::fs;
use std::path::PathBuf;

pub(crate) fn run(picomet_file_path: PathBuf) {
    let picomet_code = fs::read_to_string(picomet_file_path).unwrap();

    let result = compile(&picomet_code);
    if !result.success {
        eprintln!("{}", DocMsg::to_text(&result.msgs));
        std::process::exit(1)
    }

    picomet_lang_runtime::eval_with_stdio(&result.program);
}
