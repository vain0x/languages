use serde::Serialize;

#[derive(Serialize)]
pub struct AIdent(pub String);

#[derive(Serialize)]
pub enum ALit {
    DecimalInt(i64),
    String(String),
}

#[derive(Serialize)]
pub enum AExprKind {
    Symbol(AIdent),
    Lit(ALit),
    Call {
        callee: Box<AExpr>,
        args: Vec<AExpr>,
    },
    Use(AIdent),
}

#[derive(Serialize)]
pub struct AExpr {
    pub kind: AExprKind,
}

#[derive(Serialize)]
pub struct ARoot(pub Vec<AExpr>);
