#[macro_use]
extern crate lalrpop_util;

pub mod ast;
mod parse;
mod tokenize;

lalrpop_mod!(pub grammar);

pub use crate::{parse::parse_from_string, tokenize::Token};

#[allow(unused)]
pub(crate) mod internals {
    pub(crate) use bumpalo::{boxed::Box as BumpBox, collections::Vec as BumpVec, Bump};
    pub(crate) use std::fmt::Debug;
}

use crate::internals::*;

pub struct AModule<'b> {
    pub name: &'b str,
    pub path: &'b str,
    pub source_code: &'b str,
    pub root: ast::ARoot<'b>,
}

pub struct AProgram<'b> {
    pub modules: BumpVec<'b, AModule<'b>>,
}
