mod features;
mod lsp_handler;
mod lsp_receiver;
mod lsp_sender;
mod types;

pub(self) use self::types::*;
pub(self) use lsp_handler::LspHandler;
pub(self) use lsp_receiver::LspReceiver;
pub(self) use lsp_sender::LspSender;
use std::io;

fn init_log() {
    let log_level = if cfg!(debug_assertions) {
        log::LevelFilter::Debug
    } else {
        log::LevelFilter::Warn
    };
    env_logger::Builder::new().filter(None, log_level).init();
}

pub fn start_lsp_server() {
    init_log();

    let stdin = io::stdin();
    let stdin = stdin.lock();
    let receiver = LspReceiver::new(stdin);
    let stdout = io::stdout();
    let stdout = stdout.lock();
    let sender = LspSender::new(stdout);
    let handler = LspHandler::new(sender);
    handler.main(receiver);
}
