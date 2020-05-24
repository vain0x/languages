use std::{
    fmt::{Debug, Formatter, Result},
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
};

#[derive(Clone)]
pub(crate) struct Container<T> {
    inner: Box<Vec<T>>,
    ptr: Rc<*mut Vec<T>>,
}

impl<T> Container<T> {
    pub(crate) fn new() -> Self {
        let mut inner = Box::new(Vec::<T>::new());
        let ptr = Rc::new(Box::as_mut(&mut inner) as *mut _);
        Container { inner, ptr }
    }

    pub(crate) fn borrow(&self, index: usize) -> ElementRef<T> {
        assert!(index < self.inner.len());
        ElementRef {
            ptr: Rc::downgrade(&self.ptr),
            index,
        }
    }

    pub(crate) fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.inner[index]
    }
}

impl<T> Debug for Container<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        <Vec<T> as Debug>::fmt(&*self.inner, f)
    }
}

impl<T> Default for Container<T> {
    fn default() -> Self {
        Container::new()
    }
}

#[derive(Clone)]
pub(crate) struct ElementRef<T> {
    ptr: Weak<*mut Vec<T>>,
    index: usize,
}

impl<T> Deref for ElementRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let ptr_rc: Rc<_> = self.ptr.upgrade().unwrap();
        let vec_ptr = *Rc::as_ref(&ptr_rc) as *const Vec<T>;
        let vec_ref = unsafe { &*vec_ptr };
        &vec_ref[self.index]
    }
}
