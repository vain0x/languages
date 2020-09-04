use crate::token::token_kind::TokenKind;

/// 構文木の一要素としてのトークン
#[derive(Copy, Clone)]
pub(crate) struct SyntaxToken {
    /// トリビアではないトークンの種類のみ入る。
    kind: TokenKind,

    /// 先行トリビアの長さ
    leading_len: u16,

    /// 本体の長さ
    body_len: u16,

    /// 後続トリビアの長さ
    trailing_len: u16,
}
