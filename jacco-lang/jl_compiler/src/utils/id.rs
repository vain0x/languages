use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
};

/// 配列のインデックス
///
/// 要素の型つき。
/// 参考: [Newtype Index Pattern](https://matklad.github.io/2018/06/04/newtype-index-pattern.html)
pub(crate) struct Id<T>(u32, PhantomData<T>);

impl<T> Id<T> {
    pub(crate) const fn new(id: usize) -> Self {
        Id(id as u32, PhantomData)
    }

    pub(crate) const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl<T> From<usize> for Id<T> {
    fn from(id: usize) -> Self {
        Id::new(id)
    }
}

impl<T> From<Id<T>> for usize {
    fn from(id: Id<T>) -> usize {
        id.as_usize()
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0, PhantomData)
    }
}

impl<T> Copy for Id<T> {}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Id(u32::default(), PhantomData)
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Debug for Id<T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.0, formatter)
    }
}

impl<T> Display for Id<T> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.0, formatter)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
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
