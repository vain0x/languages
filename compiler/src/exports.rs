//! Export specific use cases of the library.

pub mod gen_rust;
pub mod tests;

// Export LSP entry point.
pub use crate::lsp::lsp_main::start_lsp_server;
