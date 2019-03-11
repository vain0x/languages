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

pub fn start_lsp_server() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let receiver = LspReceiver::new(stdin);
    let stdout = io::stdout();
    let stdout = stdout.lock();
    let sender = LspSender::new(stdout);
    let handler = LspHandler::new(sender);
    handler.main(receiver);
}
