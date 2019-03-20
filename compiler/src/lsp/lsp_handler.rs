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

    fn initialize(&mut self, json: &str) {
        let msg = serde_json::from_str::<LspRequest<InitializeParams>>(json).unwrap();

        self.sender.send_response(
            msg.id,
            InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Options(
                        TextDocumentSyncOptions {
                            open_close: Some(true),
                            change: Some(TextDocumentSyncKind::Full),
                            ..TextDocumentSyncOptions::default()
                        },
                    )),
                    completion_provider: Some(CompletionOptions {
                        resolve_provider: Some(true),
                        trigger_characters: None,
                    }),
                    hover_provider: Some(true),
                    references_provider: Some(true),
                    rename_provider: Some(RenameProviderCapability::Simple(true)),
                    ..ServerCapabilities::default()
                },
            },
        );
    }

    fn did_initialize(&self, _json: &str) {
        // Pass
    }

    fn shutdown(&mut self, json: &str) {
        let msg = serde_json::from_str::<LspRequest<()>>(json).unwrap();
        self.sender.send_response(msg.id, ());
    }

    fn did_exit(&mut self, _json: &str) {
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

    fn text_document_did_close(&mut self, json: &str) {
        let msg =
            serde_json::from_str::<LspNotification<DidCloseTextDocumentParams>>(json).unwrap();
        self.model.close_doc(&msg.params.text_document.uri);
    }

    fn text_document_completion(&mut self, json: &str) {
        let msg = serde_json::from_str::<LspRequest<CompletionParams>>(json).unwrap();
        let (id, params) = (msg.id, msg.params);

        let completion_list: CompletionList = self
            .model
            .completion(&params.text_document.uri, params.position);

        self.sender.send_response(id, completion_list);
    }

    fn completion_item_resolve(&mut self, json: &str) {
        let msg = serde_json::from_str::<LspRequest<CompletionItem>>(json).unwrap();
        let (id, completion_item) = (msg.id, msg.params);

        self.sender.send_response(id, completion_item);
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
        let msg = serde_json::from_str::<LspMessageOpaque>(json).unwrap();

        match msg.method.as_str() {
            "initialize" => self.initialize(json),
            "initialized" => self.did_initialize(json),
            "shutdown" => self.shutdown(json),
            "exit" => self.did_exit(json),
            "textDocument/didOpen" => self.text_document_did_open(json),
            "textDocument/didChange" => self.text_document_did_change(json),
            "textDocument/didClose" => self.text_document_did_close(json),
            "textDocument/completion" => self.text_document_completion(json),
            "completionItem/resolve" => self.completion_item_resolve(json),
            "textDocument/hover" => self.text_document_hover(json),
            "textDocument/references" => self.text_document_references(json),
            "textDocument/rename" => self.text_document_rename(json),
            _ => warn!("Msg unresolved."),
        }
    }

    pub fn main(mut self, mut receiver: LspReceiver<impl io::Read>) {
        loop {
            receiver.read_next(|json| self.did_receive(json));
        }
    }
}
