#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

mod lsp;
mod mir;
mod msg;
mod parse;
mod sema;
mod semantics;
mod syntax;
mod tokenize;

pub use crate::lsp::start_lsp_server;
pub use crate::mir::gen_mir::compile;
pub use crate::mir::CompilationResult;
pub(crate) use crate::mir::*;
pub use crate::msg::DocMsg;
pub(crate) use crate::msg::*;
pub(crate) use crate::semantics::*;
pub use crate::syntax::Doc;
pub(crate) use crate::syntax::*;
use std::cmp::{min, Ordering};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Formatter};
use std::iter;
use std::marker::PhantomData;
use std::rc::Rc;
use std::str;

pub struct Id<T>(usize, PhantomData<T>);

impl<T> Id<T> {
    const fn new(id: usize) -> Self {
        Id(id, PhantomData)
    }
}

impl<T> From<usize> for Id<T> {
    fn from(id: usize) -> Self {
        Id::new(id)
    }
}

impl<T> From<Id<T>> for usize {
    fn from(id: Id<T>) -> usize {
        id.0
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id::new(self.0)
    }
}

impl<T> Copy for Id<T> {}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Id::new(usize::default())
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Debug for Id<T> {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> std::ops::Add<usize> for Id<T> {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        Id::from(self.0 + rhs)
    }
}

impl<T> std::ops::AddAssign<usize> for Id<T> {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl<T> std::ops::Sub<usize> for Id<T> {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self {
        Id::from(self.0 - min(self.0, rhs))
    }
}

pub(crate) struct RegTag;
pub(crate) struct LabelTag;

type MsgId = Id<Msg>;
type TokenId = Id<Token>;
type ExpId = Id<Exp>;
type RegId = Id<RegTag>;
type LabelId = Id<LabelTag>;
type VarId = Id<VarDef>;
type FunId = Id<FunDef>;
