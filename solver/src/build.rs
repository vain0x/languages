//! This program runs whenever you build this project.
//! Compile `main.picomet` and bundle the result and the runtime code into single Rust code.

extern crate picomet_lang_compiler;

use std::fs;
use std::io::prelude::*;
use std::path::Path;

const TEMPLATE: &'static str = r#####"// picomet-lang <https://github.com/vain0x/picomet-lang>

const PROGRAM: &'static str = r####"
${IR_CONTENTS}"####;

pub fn main() {
    eval_with_stdio(PROGRAM);
}

${RUNTIME_CONTENTS}"#####;

fn read_to_string(p: &Path) -> String {
    let mut file = match fs::File::open(p) {
        Ok(file) => file,
        Err(_) => return "".into(),
    };
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    buffer
}

fn main() {
    let root_path = env!("CARGO_MANIFEST_DIR");
    let runtime_path = Path::new(root_path)
        .join("..")
        .join("runtime")
        .join("src")
        .join("lib.rs");
    let picomet_path = Path::new(root_path).join("src").join("main.picomet");
    let out_path = Path::new(root_path).join("src").join("main.rs");

    let runtime_contents = read_to_string(&runtime_path);
    let picomet_contents = read_to_string(&picomet_path);
    let current_contents = read_to_string(&out_path);

    // FIXME: It should not panic on compile error.
    let ir_contents =
        std::panic::catch_unwind(|| picomet_lang_compiler::compile(&picomet_contents))
            .unwrap_or("  Exit 0 0".to_owned());

    let out_contents = TEMPLATE
        .replace("${IR_CONTENTS}", &ir_contents)
        .replace("${RUNTIME_CONTENTS}", &runtime_contents);

    if current_contents != out_contents {
        let mut file = fs::File::create(out_path).unwrap();
        file.write_all(out_contents.as_bytes()).unwrap();
    }
}
