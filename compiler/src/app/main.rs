use crate::app;
use crate::lsp;
use std::env::ArgsOs;
use std::path::PathBuf;

enum Arg {
    Help,
    Version,
    Run {
        picomet_file_path: PathBuf,
    },
    GenRust {
        picomet_file_path: PathBuf,
        runtime_file_path: PathBuf,
        out_file_path: PathBuf,
    },
    Lsp,
}

fn get_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn get_help() -> String {
    format!(
        r#"picomet {version}
USAGE:
    picomet [OPTIONS] [SUBCOMMAND]

SUBCOMMAND:
    run [PICOMET_FILE_PATH]
        Run Picomet code

    gen_rust [PICOMET_FILE] [RUNTIME_FILE] [OUT_FILE]
        Bundle Picomet code and Runtime
        to single Rust code

    lsp
        Start LSP server

OPTIONS:
    -h, --help      Print help
    -V, --version   Print Version"#,
        version = get_version()
    )
}

fn exit_with_version() -> ! {
    eprintln!("{}", get_version());
    std::process::exit(1)
}

fn exit_with_help() -> ! {
    eprintln!("{}", get_help());
    std::process::exit(1)
}

fn parse_args(args: ArgsOs) -> Result<Arg, String> {
    let argc = args.len();

    let mut args = args.into_iter();
    let verb = match args.next() {
        None => return Ok(Arg::Help),
        Some(verb) => verb,
    };

    match verb.into_string().unwrap().as_str() {
        "-h" | "--help" | "help" => Ok(Arg::Help),
        "-V" | "--version" | "version" => Ok(Arg::Version),
        "run" => {
            if argc < 2 {
                return Err("run: Missing some parameter.".to_string());
            }
            Ok(Arg::Run {
                picomet_file_path: PathBuf::from(args.next().unwrap()),
            })
        }
        "gen_rust" => {
            if argc < 4 {
                return Err("gen_rust: Missing some parameter.".to_string());
            }
            Ok(Arg::GenRust {
                picomet_file_path: PathBuf::from(args.next().unwrap()),
                runtime_file_path: PathBuf::from(args.next().unwrap()),
                out_file_path: PathBuf::from(args.next().unwrap()),
            })
        }
        "lsp" => Ok(Arg::Lsp),
        verb => Err(format!("Unknown subcommand '{}'.", verb)),
    }
}

fn switch_on_args(mut args: ArgsOs) {
    // Skip self path.
    args.next();

    let arg = parse_args(args).unwrap_or_else(|err| {
        eprintln!("{}", err);
        exit_with_help();
    });

    match arg {
        Arg::Version => exit_with_version(),
        Arg::Help => exit_with_help(),
        Arg::Run { picomet_file_path } => app::run::run(picomet_file_path),
        Arg::GenRust {
            picomet_file_path,
            runtime_file_path,
            out_file_path,
        } => {
            app::gen_rust::gen_rust_to_file(&picomet_file_path, &runtime_file_path, &out_file_path)
                .unwrap()
        }
        Arg::Lsp => lsp::lsp_main::start_lsp_server(),
    }
}

pub fn main() {
    switch_on_args(std::env::args_os())
}
