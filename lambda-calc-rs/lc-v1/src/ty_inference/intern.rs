/// インターン化されたオブジェクトへの参照。
///
/// 参照的な同値性を備える。
pub(crate) struct Interned<'a, T> {
    inner: &'a T,
}

impl<'a, T> Interned<'a, T> {
    pub(crate) fn get(&self) -> &'a T {
        self.inner
    }
}

impl<'a, T> Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<'a, T> Clone for Interned<'a, T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner }
    }
}

impl<'a, T> Copy for Interned<'a, T> {}

impl<'a, T> PartialEq for Interned<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.inner, other.inner)
    }
}

impl<'a, T> Eq for Interned<'a, T> {}

impl<'a, T> Hash for Interned<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.inner, state);
    }
}

pub(crate) struct Interner<'a, T> {
    set: RefCell<HashSet<&'a T>>,
    bump: &'a Bump,
}

impl<'a, T: Eq + Hash> Interner<'a, T> {
    pub(crate) fn intern(&self, value: T) -> Interned<'a, T> {
        let mut set = self.set.borrow_mut();
        match set.get(&value) {
            Some(it) => Interned { inner: it },
            None => {
                let value = self.bump.alloc(value);
                set.insert(value);
                Interned { inner: value }
            }
        }
    }
}
