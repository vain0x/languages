use crate::lsp::*;
use lsp_types::*;
use std::io;

pub(super) struct LspHandler<W: io::Write> {
    sender: LspSender<W>,
    model: LspModel,
}

impl<W: io::Write> LspHandler<W> {
    pub fn new(sender: LspSender<W>) -> Self {
        Self {
            sender,
            model: LspModel::new(),
        }
    }

    fn did_initialize(&mut self, id: i64) {
        self.sender.send_response(
            id,
            InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Options(
                        TextDocumentSyncOptions {
                            open_close: Some(true),
                            change: Some(TextDocumentSyncKind::Full),
                            ..TextDocumentSyncOptions::default()
                        },
                    )),
                    hover_provider: Some(true),
                    references_provider: Some(true),
                    rename_provider: Some(RenameProviderCapability::Simple(true)),
                    ..ServerCapabilities::default()
                },
            },
        );
    }

    fn did_shutdown(&mut self, id: i64) {
        self.sender.send_response(id, ());
    }

    fn did_exit(&mut self) {
        std::process::exit(0)
    }

    fn text_document_did_open(&mut self, json: &str) {
        let n: LspNotification<DidOpenTextDocumentParams> =
            serde_json::from_str(&json).expect("did open notification");
        let doc = n.params.text_document;
        let uri = doc.uri.to_owned();
        self.model.open_doc(doc.uri, doc.version, doc.text);

        self.text_document_did_open_or_change(&uri);
    }

    fn text_document_did_change(&mut self, json: &str) {
        let n: LspNotification<DidChangeTextDocumentParams> =
            serde_json::from_str(&json).expect("did change notification");

        let text = (n.params.content_changes.into_iter())
            .next()
            .map(|c| c.text)
            .unwrap_or("".to_string());

        let doc = n.params.text_document;
        let uri = doc.uri.to_owned();
        let version = doc.version.unwrap_or(0);

        self.model.change_doc(doc.uri, version, text);

        self.text_document_did_open_or_change(&uri);
    }

    fn text_document_did_open_or_change(&mut self, uri: &Url) {
        let diagnostics = self.model.validate(uri);

        self.sender.send_notification(
            "textDocument/publishDiagnostics",
            PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
            },
        );
    }

    fn text_document_hover(&mut self, json: &str) {
        let request: LspRequest<TextDocumentPositionParams> = serde_json::from_str(json).unwrap();

        let hover: Option<Hover> = self
            .model
            .hover(&request.params.text_document.uri, request.params.position);

        self.sender.send_response(request.id, hover);
    }

    fn text_document_references(&mut self, json: &str) {
        let request: LspRequest<ReferenceParams> = serde_json::from_str(json).unwrap();

        let locations: Vec<Location> = self.model.references(
            &request.params.text_document.uri,
            request.params.position,
            request.params.context.include_declaration,
        );

        self.sender.send_response(request.id, locations);
    }

    fn text_document_rename(&mut self, json: &str) {
        let request: LspRequest<RenameParams> = serde_json::from_str(json).unwrap();

        let edit: Option<WorkspaceEdit> = self.model.rename(
            &request.params.text_document.uri,
            request.params.position,
            request.params.new_name,
        );

        self.sender.send_response(request.id, edit);
    }

    fn did_receive(&mut self, json: &str) {
        let mut id = None;
        if let Some(mut l) = json.find(r#""id":"#) {
            l += r#""id":"#.len();
            if let Some(n) = json[l..].find(",") {
                id = json[l..l + n].trim().parse::<i64>().ok();
            }
        }

        // FIXME: use deserializer
        if json.contains("initialized") {
            // Pass.
        } else if json.contains("initialize") {
            self.did_initialize(id.unwrap());
        } else if json.contains("shutdown") {
            self.did_shutdown(id.unwrap());
        } else if json.contains("exit") {
            self.did_exit();
        } else if json.contains("textDocument/didOpen") {
            self.text_document_did_open(json);
        } else if json.contains("textDocument/didChange") {
            self.text_document_did_change(json);
        } else if json.contains("textDocument/hover") {
            self.text_document_hover(json);
        } else if json.contains("textDocument/references") {
            self.text_document_references(json);
        } else if json.contains("textDocument/rename") {
            self.text_document_rename(json);
        } else {
            warn!("Msg unresolved.")
        }
    }

    pub fn main(mut self, mut receiver: LspReceiver<impl io::Read>) {
        loop {
            receiver.read_next(|json| self.did_receive(json));
        }
    }
}
