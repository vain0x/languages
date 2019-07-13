fn main() {
    let entry_path = std::env::args()
        .skip(1)
        .next()
        .expect("Expect an argument <entry-path>");
    let output = al_compiler::build(&entry_path).unwrap();
    eprint!("{}", output.msg);
    print!("{}", output.il);
}
