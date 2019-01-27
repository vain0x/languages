extern crate picomet_lang_runtime;

const PROGRAM: &'static str = r#####"
    Imm 0 1
    ToStr 0 0
    PrintLn 0
    Exit 0
"#####;

fn main() {
    picomet_lang_runtime::eval_with_stdio(PROGRAM);
}
