//! Export specific use cases of the library.

pub mod gen_rust;
pub mod tests;
pub mod run;

// Export LSP entry point.
pub use crate::lsp::lsp_main::start_lsp_server;
