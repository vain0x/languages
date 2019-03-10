mod mir;
mod msg;
mod parse;
mod sema;
mod semantics;
mod syntax;
mod tokenize;

pub(crate) use crate::mir::*;
pub use crate::mir::{gen_mir::compile, CompilationResult};
pub(crate) use crate::msg::*;
pub(crate) use crate::semantics::*;
pub(crate) use crate::syntax::*;
use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::iter;
use std::rc::Rc;
use std::str;

macro_rules! define_rich_id {
    ($($name:ident),*) => {
        $(
            #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
            pub struct $name(usize);

            impl From<usize> for $name {
                fn from(id: usize) -> Self {
                    $name(id)
                }
            }

            impl From<$name> for usize {
                fn from(id: $name) -> usize {
                    id.0
                }
            }

            impl std::ops::Add<usize> for $name {
                type Output = Self;

                fn add(self, rhs: usize) -> Self {
                    $name(self.0 + rhs)
                }
            }

            impl std::ops::AddAssign<usize> for $name {
                fn add_assign(&mut self, rhs: usize)  {
                    self.0 += rhs;
                }
            }

            impl std::ops::Sub<usize> for $name {
                type Output = Self;

                fn sub(self, rhs: usize) -> Self {
                    $name(self.0 - min(self.0, rhs))
                }
            }
        )*
    };
}

define_rich_id!(MsgId, TokenId, ExpId, SymbolId, RegId, LabelId, VarId, FunId);
