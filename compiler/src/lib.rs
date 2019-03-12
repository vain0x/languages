#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

mod lsp;
mod mir;
mod semantics;
mod syntax;
mod tests;
mod util;

pub use crate::lsp::lsp_main::start_lsp_server;
pub use crate::mir::gen_mir::compile;
pub use crate::mir::CompilationResult;
pub use crate::semantics::msg::DocMsg;
pub use crate::syntax::Doc;
pub use crate::tests::helpers::{eval_tests, test_err};

pub(crate) use crate::util::id::Id;
