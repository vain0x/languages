//! Convert picomet-lang code to a string to execute with Rust tools.

use crate::mir::gen_mir::compile;
use crate::semantics::msg::DocMsg;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::Path;

const TEMPLATE: &'static str = r#####"// picomet-lang <https://github.com/vain0x/picomet-lang>

#[allow(unused)]
const SOURCE: &'static str = r####"
${SOURCE}"####;

const PROGRAM: &'static str = r####"
${MIR}"####;

pub fn main() {
    eval_with_stdio(PROGRAM);
}

${RUNTIME}"#####;

/// Compile the picomet code and bundle it with runtime code.
/// The result is single Rust code that interprets the given picomet code.
fn gen_rust(picomet_code: String, runtime_code: String) -> String {
    let result = compile(&picomet_code);
    if result.success {
        TEMPLATE
            .replace("${SOURCE}", &picomet_code)
            .replace("${MIR}", &result.program)
            .replace("${RUNTIME}", &runtime_code)
    } else {
        let stderr = DocMsg::to_text(&result.msgs);
        let err_code = format!(r##"compile_error!(r#"{}"#)"##, &stderr);
        TEMPLATE
            .replace("${SOURCE}", &picomet_code)
            .replace("${MIR}", "")
            .replace("${RUNTIME}", &err_code)
    }
}

fn read_to_string(path: &Path) -> io::Result<String> {
    let mut file = fs::File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

pub(crate) fn gen_rust_to_file(
    picomet_path: &Path,
    runtime_path: &Path,
    out_path: &Path,
) -> io::Result<()> {
    let runtime_contents = read_to_string(&runtime_path)?;
    let picomet_contents = read_to_string(&picomet_path)?;
    let current_contents = read_to_string(&out_path).unwrap_or("".to_string());

    let out_contents = gen_rust(picomet_contents, runtime_contents);

    if current_contents != out_contents {
        let mut file = fs::File::create(out_path)?;
        file.write_all(out_contents.as_bytes())?;
    }

    Ok(())
}
