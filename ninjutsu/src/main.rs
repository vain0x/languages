use ninjutsu::{
    parse,
    tokenize::{self, Tokenizer},
};
use std::{
    env::args,
    fs,
    os::unix::prelude::CommandExt,
    path::PathBuf,
    process::{exit, Command},
};

fn help() -> ! {
    println!("ninjutsu <SUBCOMMAND>");
    exit(0)
}

fn main() {
    let mut args = args();
    args.next();

    match args.next().as_deref().unwrap_or("--help") {
        "--help" | "-h" | "help" => help(),

        "tokenize" => {
            for filename in args {
                let filename = PathBuf::from(filename);
                let buf = fs::read_to_string(&filename).expect("read");
                let tokens = tokenize::tokenize_to_vec(&buf);
                let w = fs::OpenOptions::new()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open(filename.with_extension("nin_tokens"))
                    .expect("write");
                let mut serializer = serde_json::Serializer::new(w);
                serde::Serialize::serialize(&tokens, &mut serializer).expect("serialize");
            }
        }
        "parse" => {
            for filename in args {
                let filename = PathBuf::from(filename);
                let buf = fs::read_to_string(&filename).expect("read");

                let tokenizer = Tokenizer::new(&buf);
                let root = parse::parse_tokens(tokenizer);

                let w = fs::OpenOptions::new()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open(filename.with_extension("nin_ast"))
                    .expect("write");

                let mut serializer = serde_json::Serializer::new(w);
                serde::Serialize::serialize(&root, &mut serializer).expect("serialize");
            }
        }
        "build" => {
            let ninja = r#"
rule cargo-build
  command = cargo build

rule parse
  command = cargo run -- parse $in

rule compile
  command = echo $in >$out

build tests/hello/hello.nin_ast: parse tests/hello/hello.nin
build tests/hello/io.nin_ast: parse tests/hello/io.nin

build tests/hello/io.nin_ir: compile tests/hello/io.nin_ast
build tests/hello/hello.nin_ir: compile tests/hello/hello.nin_ast tests/hello/io.nin_ir

default tests/hello/hello.nin_ir
"#;

            let build_file = &"tests/hello/build.ninja";
            fs::write(build_file, ninja).expect("write");
            let err = Command::new("./bin/ninja").arg("-f").arg(build_file).arg("-v").exec();
            panic!("{:?}", err);
        }

        arg => {
            eprintln!("Unknown subcommand '{}'.", arg);
            exit(1)
        }
    }
}
