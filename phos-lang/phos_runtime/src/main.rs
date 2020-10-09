mod il_parse;
mod il_types;
mod rt_val;
mod rt_vm;

pub(crate) use il_parse::*;
pub(crate) use il_types::*;
pub(crate) use rt_val::*;
pub(crate) use rt_vm::*;

use std::{
    cell::Cell,
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

fn main() {
    let buf = {
        use std::io::Read;

        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf).unwrap();
        Box::leak(buf.into_boxed_str())
    };

    let m = parse_pil(buf, "stdin".to_string(), "stdin".to_string());
    Vm::new(m).run();
}
