use super::*;

pub(crate) fn parse(text: &str) -> Vec<Code> {
    let mut codes = Vec::new();
    let tokens = text.split_ascii_whitespace().collect::<Vec<_>>();
    let mut i = 0;

    while i < tokens.len() {
        i += 1;
        match tokens[i - 1] {
            "exit" => codes.push(Code::Exit),
            "assert" => codes.push(Code::Assert),
            "true" => codes.push(Code::PushTrue),
            "false" => codes.push(Code::PushFalse),
            "push_int" => {
                i += 1;
                codes.push(Code::PushInt(tokens[i - 1].parse::<i64>().unwrap()));
            }
            "op_add" => codes.push(Code::OpAdd),
            "op_eq" => codes.push(Code::OpEq),
            _ => unreachable!("Unknown IL token {}", tokens[i - 1])
        }
    }

    codes
}
