use crate::compile;
use lsp_types::*;
use serde_json;
use std::io::*;

#[derive(Serialize, Deserialize)]
struct LspResponse<Result> {
    jsonrpc: String,
    id: i64,
    result: Result,
}

#[derive(Serialize, Deserialize)]
struct LspNotification<Params> {
    jsonrpc: String,
    method: String,
    params: Params,
}

fn send_lsp_msg(content: &str) {
    let content_length = content.len();

    let stdout = stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    write!(
        stdout,
        "Content-Length: {}\r\n\r\n{}",
        content_length, content
    )
    .unwrap();
    stdout.flush().unwrap();

    eprintln!("Send Content-Length: {}\r\n\r\n{}", content_length, content);
}

fn did_receive(json: &str) {
    let mut id = None;
    if let Some(mut l) = json.find(r#""id":"#) {
        l += r#""id":"#.len();
        if let Some(n) = json[l..].find(",") {
            id = json[l..l + n].trim().parse::<i64>().ok();
        }
    }

    if json.contains("initialized") {
        // pass
    } else if json.contains("initialize") {
        let response = LspResponse::<InitializeResult> {
            jsonrpc: "2.0".to_string(),
            id: id.unwrap(),
            result: InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Options(
                        TextDocumentSyncOptions {
                            open_close: Some(true),
                            change: Some(TextDocumentSyncKind::Full),
                            ..TextDocumentSyncOptions::default()
                        },
                    )),
                    ..ServerCapabilities::default()
                },
            },
        };
        send_lsp_msg(&serde_json::to_string(&response).unwrap());
    } else if json.contains("shutdown") {
        let response = LspResponse::<()> {
            jsonrpc: "2.0".to_string(),
            id: id.unwrap(),
            result: (),
        };
        send_lsp_msg(&serde_json::to_string(&response).unwrap());
    } else if json.contains("exit") {
        std::process::exit(0)
    } else if json.contains("textDocument/didOpen") {
        let n: LspNotification<DidOpenTextDocumentParams> =
            serde_json::from_str(&json).expect("did open notification");

        validate_document(&n.params.text_document.uri, &n.params.text_document.text);
    } else if json.contains("textDocument/didChange") {
        let n: LspNotification<DidChangeTextDocumentParams> =
            serde_json::from_str(&json).expect("did change notification");

        let text = (n.params.content_changes.first())
            .map(|c| c.text.as_ref())
            .unwrap_or("");

        validate_document(&n.params.text_document.uri, text);
    } else {
        eprintln!("Msg unresolved.")
    }
}

fn validate_document(uri: &Url, src: &str) {
    let result = compile(src);
    let mut diagnostics = vec![];
    for msg in &result.msgs {
        diagnostics.push(Diagnostic {
            range: Range {
                start: Position {
                    line: 1,
                    character: 1,
                },
                end: Position {
                    line: 1,
                    character: 2,
                },
            },
            message: msg.message().to_string(),
            source: Some("Picomet-lang LSP".to_string()),
            ..Diagnostic::default()
        })
    }

    let n = LspNotification::<PublishDiagnosticsParams> {
        jsonrpc: "2.0".to_string(),
        method: "textDocument/publishDiagnostics".to_string(),
        params: PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics,
        },
    };

    send_lsp_msg(&serde_json::to_string(&n).unwrap());
}

pub fn start() {
    let mut line = String::new();
    let mut content = Vec::new();
    let stdin = stdin();
    let mut stdin = BufReader::new(stdin.lock());

    loop {
        line.clear();
        stdin.read_line(&mut line).expect("read header");
        if !line.starts_with("Content-Length:") {
            panic!("Unknown header {}", line);
        }

        let l = "Content-Length:".len();
        let r = line.len();
        let content_length = line[l..r]
            .trim()
            .parse::<usize>()
            .expect("content length to be integer");

        line.clear();
        stdin.read_line(&mut line).expect("read empty");
        if line.trim().len() != 0 {
            panic!("Unknown header {}", line);
        }

        content.resize(content_length, 0);
        stdin.read_exact(&mut content).expect("read payload");

        let json = String::from_utf8_lossy(&content);

        eprintln!("Received {}\n", json);

        did_receive(&json);
    }
}
