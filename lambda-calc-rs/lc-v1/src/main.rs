use lc_v1;
use rustyline::{error::ReadlineError, Editor};

fn on_read(s: &str) {
    let output = lc_v1::rust_api::evaluate(s);
    println!("{}", output);
}

fn main() {
    let mut rl: Editor<()> = Editor::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.to_string());
                on_read(&line);
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
}
