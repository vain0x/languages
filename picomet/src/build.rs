use std::fs;
use std::io::prelude::*;
use std::path::Path;

const TEMPLATE: &'static str = r#####"// This program is compiled and evaluated at runtime.
const PROGRAM: &'static str = r####"
${PICOMET_CONTENTS}"####;

pub fn main() {
    eval_with_stdio(PROGRAM);
}

${LIB_CONTENTS}"#####;

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
    let lib_path = Path::new(root_path).join("src").join("lib.rs");
    let picomet_path = Path::new(root_path).join("examples").join("solver.picomet");
    let solver_path = Path::new(root_path).join("examples").join("solver.rs");

    let lib_contents = read_to_string(&lib_path);
    let picomet_contents = read_to_string(&picomet_path);
    let current_contents = read_to_string(&solver_path);

    let solver_contents = TEMPLATE
        .replace("${PICOMET_CONTENTS}", &picomet_contents)
        .replace("${LIB_CONTENTS}", &lib_contents);

    if current_contents != solver_contents {
        let mut file = fs::File::create(solver_path).unwrap();
        file.write_all(solver_contents.as_bytes()).unwrap();
    }
}
