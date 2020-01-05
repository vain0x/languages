#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

mod app;
mod emit_js;
mod lsp;
mod pir;
mod rir;
mod semantics;
mod syntax;
mod util;

pub(crate) use crate::util::id::Id;

// Export the module as top level of the library.
pub use app::*;
