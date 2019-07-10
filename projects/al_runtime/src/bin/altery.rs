use std::fs;

fn main() {
    let il_path = std::env::args_os()
        .skip(1)
        .next()
        .expect("Expect an argument <il-path>");
    let il = fs::read_to_string(il_path).unwrap();
    al_runtime::run(&il);
}
