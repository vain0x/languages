// 意味検査の対象となる中間表現。

pub mod xir;

#[allow(unused)]
pub(crate) mod internals {
    pub(crate) use bumpalo::{
        boxed::Box as BumpBox, collections::Vec as BumpVec,
        core_alloc::string::String as BumpString, Bump,
    };
    pub(crate) use std::cell::RefCell;
    pub(crate) use std::collections::HashMap;
    pub(crate) use std::fmt::{self, Debug, Display};
    pub(crate) use std::mem::{replace, swap, take};
}

use crate::internals::*;
