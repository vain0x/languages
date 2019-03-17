//! This program runs whenever you build this project.
//! Compile `main.picomet` and bundle the result and the runtime code into single Rust code.

extern crate picomet_lang_compiler;

use std::fs;
use std::io::prelude::*;
use std::path::Path;
use picomet_lang_compiler::gen_rust::{gen_rust_to_file};

fn main() {
    let root_path = env!("CARGO_MANIFEST_DIR");
    let runtime_path = Path::new(root_path)
        .join("../runtime/src/lib.rs");
    let picomet_path = Path::new(root_path).join("src").join("main.picomet");
    let out_path = Path::new(root_path).join("src").join("main.rs");

    gen_rust_to_file(&picomet_path, &runtime_path, &out_path);
}
