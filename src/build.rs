use std::fs;
use std::path::Path;

const TEMPLATE: &'static str = r#####"const PROGRAM: &'static str = r####"
${ATLISP_CONTENTS}"####;

pub fn main() {
    eval_with_stdio(PROGRAM.into());
}

${LIB_CONTENTS}"#####;

fn main() {
    let root_path = env!("CARGO_MANIFEST_DIR");
    let lib_path = Path::new(root_path).join("src").join("lib.rs");
    let atlisp_path = Path::new(root_path).join("examples").join("solver.atlisp");
    let solver_path = Path::new(root_path).join("examples").join("solver.rs");

    let lib_contents = fs::read_to_string(&lib_path).unwrap();
    let atlisp_contents = fs::read_to_string(&atlisp_path).unwrap();
    let current_contents = fs::read_to_string(&solver_path).unwrap_or_default();

    let solver_contents = TEMPLATE.replace("${ATLISP_CONTENTS}", &atlisp_contents).replace("${LIB_CONTENTS}", &lib_contents);

    if current_contents != solver_contents {
        fs::write(solver_path, solver_contents).unwrap();
    }
}
