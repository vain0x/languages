#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

mod exports;
mod lsp;
mod mir;
mod semantics;
mod syntax;
mod util;

pub(crate) use crate::util::id::Id;

// Export the module as top level of the library.
pub use crate::exports::*;
