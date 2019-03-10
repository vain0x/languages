use crate::compile;
use serde_json;
use std::io::*;

struct LspMsg {
    jsonrpc: String,
    id: i64,
}

#[derive(Deserialize)]
struct LspNotification<Params> {
    jsonrpc: String,
    method: String,
    params: Params,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct DidOpenTextDocumentParams {
    text_document: TextDocument,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct DidChangeTextDocumentParams {
    text_document: VersionedTextDocumentIdentifier,
    content_changes: Vec<TextDocumentContentChangeEvent>,
}

#[derive(Deserialize)]
struct TextDocumentContentChangeEvent {
    text: String,
}

#[derive(Serialize, Deserialize)]
struct VersionedTextDocumentIdentifier {
    uri: String,
    version: Option<usize>,
}

#[derive(Deserialize)]
struct TextDocument {
    uri: String,
    version: i64,
    text: String,
}

#[derive(Serialize)]
struct Diagnostic {
    range: Range,
    source: String,
    message: String,
}

#[derive(Serialize, Deserialize)]
struct Range {
    start: Position,
    end: Position,
}

#[derive(Serialize, Deserialize)]
struct Position {
    line: usize,
    character: usize,
}

static mut ID: usize = 1;

fn send_lsp_msg(content: &str) {
    let content_length = content.len();

    let stdout = stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    write!(stdout, "Content-Length: {}\r\n\r\n{}", content_length, content);
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
        send_lsp_msg(&format!(
            r#"
                {{
                    "jsonrpc": "2.0",
                    "id": {id},
                    "result": {{
                        "capabilities": {{
                            "textDocumentSync": {{
                                "openClose": true,
                                "change": 1
                            }}
                        }}
                    }}
                }}
            "#,
            id = id.unwrap()
        ));
    } else if json.contains("shutdown") {
        send_lsp_msg(&format!(
            r#"
                {{
                    "jsonrpc": "2.0",
                    "id": {},
                    "result": null
                }}
            "#,
            id = id.unwrap()
        ))
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

fn validate_document(uri: &str, src: &str) {
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
            source: "Picomet-lang LSP".to_string(),
        })
    }

    let diagnostics = serde_json::to_string(&diagnostics).expect("Serialize diagnostics");

    send_lsp_msg(&format!(
        r#"
            {{
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": {{
                    "uri": "{uri}",
                    "diagnostics": {diagnostics}
                }}
            }}
        "#,
        uri = uri,
        diagnostics = diagnostics,
    ));
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
