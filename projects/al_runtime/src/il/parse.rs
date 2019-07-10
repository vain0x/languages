use super::*;

pub(crate) fn parse(text: &str) -> Vec<Code> {
    let mut codes = Vec::new();

    for token_text in text.split_ascii_whitespace() {
        if token_text == "exit" {
            codes.push(Code::Exit);
        } else if token_text == "assert" {
            codes.push(Code::Assert);
        } else if token_text == "true" {
            codes.push(Code::PushTrue);
        }
    }
    codes
}
