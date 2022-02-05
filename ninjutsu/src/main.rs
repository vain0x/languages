use ninjutsu::{
    parse,
    tokenize::{self, Tokenizer},
};
use std::{env::args, fs, path::PathBuf, process::exit};

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

        arg => {
            eprintln!("Unknown subcommand '{}'.", arg);
            exit(1)
        }
    }
}
