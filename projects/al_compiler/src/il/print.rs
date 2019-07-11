use crate::il::*;
use std::io::{self, Write};

pub(crate) fn print(codes: &[Code]) -> io::Result<Vec<u8>> {
    let mut out = Vec::new();

    for code in codes {
        match code {
            Code::Exit => writeln!(out, "exit")?,
            Code::Assert => writeln!(out, "assert")?,
            Code::PushTrue => writeln!(out, "true")?,
            Code::PushInt(value) => writeln!(out, "push_int {}", value)?,
            Code::OpEq => writeln!(out, "op_eq")?,
        }
    }

    Ok(out)
}
