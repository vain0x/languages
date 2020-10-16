use std::{hash::Hash, hash::Hasher, ops::Deref, ptr};

/// 内的可変性を持つデータへの参照。
///
/// 参照的な同値性を備える。
pub(crate) struct Ref<'a, T> {
    inner: &'a T,
}

impl<'a, T> Ref<'a, T> {
    pub(crate) fn new(inner: &'a T) -> Self {
        Self { inner }
    }

    pub(crate) fn as_ref(&self) -> &'a T {
        self.inner
    }
}

impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        Ref { inner: self.inner }
    }
}

impl<'a, T> Copy for Ref<'a, T> {}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<'a, T> PartialEq for Ref<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.inner, other.inner)
    }
}

impl<'a, T> Eq for Ref<'a, T> {}

impl<'a, T> Hash for Ref<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.inner, state)
    }
}
