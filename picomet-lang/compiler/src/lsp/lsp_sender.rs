use crate::lsp::*;
use std::io;
use std::io::Write as _;

pub(super) struct LspSender<W: io::Write> {
    out: io::BufWriter<W>,
}

impl<W: io::Write> LspSender<W> {
    fn do_send(&mut self, content: &[u8]) {
        let content_length = content.len();
        let content = String::from_utf8_lossy(content);

        write!(
            self.out,
            "Content-Length: {}\r\n\r\n{}",
            content_length, content
        )
        .unwrap();
        self.out.flush().unwrap();

        debug!(
            "lsp-sender/send Content-Length: {}\r\n\r\n{}",
            content_length, content
        );
    }

    pub fn send_notification<P: serde::Serialize>(&mut self, method: &str, params: P) {
        let mut buf = Vec::new();
        serde_json::to_writer(
            &mut buf,
            &LspNotification::<P> {
                jsonrpc: "2.0".to_string(),
                method: method.to_string(),
                params,
            },
        )
        .unwrap();

        self.do_send(&buf);
    }

    pub fn send_response<R: serde::Serialize>(&mut self, id: i64, result: R) {
        let mut buf = Vec::new();
        serde_json::to_writer(
            &mut buf,
            &LspResponse::<R> {
                jsonrpc: "2.0".to_string(),
                id,
                result,
            },
        )
        .unwrap();

        self.do_send(&buf);
    }

    pub fn new(out: W) -> LspSender<W> {
        LspSender {
            out: io::BufWriter::new(out),
        }
    }
}
