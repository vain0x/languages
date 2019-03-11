#[derive(Serialize, Deserialize)]
pub(super) struct LspResponse<Result> {
    pub jsonrpc: String,
    pub id: i64,
    pub result: Result,
}

#[derive(Serialize, Deserialize)]
pub(super) struct LspNotification<Params> {
    pub jsonrpc: String,
    pub method: String,
    pub params: Params,
}
