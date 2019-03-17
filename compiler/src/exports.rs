//! Export specific use cases of the library.

pub mod tests;

// Export LSP entry point.
pub use crate::lsp::lsp_main::start_lsp_server;

// Export compilation functionalities.
pub use crate::mir::gen_mir::compile;
pub use crate::mir::CompilationResult;
pub use crate::semantics::msg::DocMsg;
