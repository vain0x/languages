/// 計算結果のキャッシュを持つためのセル。
///
/// 同値性の判定において無視される。
#[derive(Default)]
pub(crate) struct CacheCell<T> {
    inner: Cell<T>,
}

impl<T: Copy> CacheCell<T> {
    pub(crate) fn get(&self) -> T {
        self.inner.get()
    }

    pub(crate) fn set(&self, value: T) {
        self.inner.set(value)
    }
}

impl<T> PartialEq for CacheCell<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for CacheCell<T> {}

impl<T> Hash for CacheCell<T> {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}
