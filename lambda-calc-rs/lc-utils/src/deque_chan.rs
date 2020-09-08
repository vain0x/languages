use std::{collections::VecDeque, marker::PhantomData};

// 未使用

// 参考: [PhantomData - The Rustonomicon](https://doc.rust-lang.org/nomicon/phantom-data.html)

/// 単一のデックへの可変参照を追加用と取り出し用の2つに分割する。
pub fn deque_chan<'a, T>(deque: &'a mut VecDeque<T>) -> (DequeSender<'a, T>, DequeReceiver<'a, T>) {
    let sender = DequeSender {
        deque: deque as *mut _,
        phantom: PhantomData,
    };
    let receiver = DequeReceiver {
        deque: deque as *mut _,
        phantom: PhantomData,
    };
    (sender, receiver)
}

/// デックへの追加専用の参照。
pub struct DequeSender<'a, T> {
    deque: *mut VecDeque<T>,

    // 'a に関して不変、T に関して反変。
    phantom: PhantomData<(&'a mut (), fn(T))>,
}

impl<'a, T> DequeSender<'a, T> {
    pub fn push_front(&mut self, value: T) {
        unsafe { &mut *self.deque }.push_front(value);
    }

    pub fn push_back(&mut self, value: T) {
        unsafe { &mut *self.deque }.push_back(value);
    }
}

/// デックからの取り出し専用の参照。
pub struct DequeReceiver<'a, T> {
    deque: *mut VecDeque<T>,

    // 'a に関して不変、T に関して共変。
    phantom: PhantomData<(&'a mut T, T)>,
}

impl<'a, T> DequeReceiver<'a, T> {
    pub fn pop_front(&mut self) -> Option<T> {
        unsafe { &mut *self.deque }.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<T> {
        unsafe { &mut *self.deque }.pop_back()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut deque = VecDeque::new();

        {
            let (mut tx, mut rx) = deque_chan(&mut deque);

            assert_eq!(rx.pop_front(), None);

            tx.push_back(1);
            tx.push_back(2);
            tx.push_back(3);

            assert_eq!(rx.pop_front(), Some(1));
            assert_eq!(rx.pop_front(), Some(2));
            assert_eq!(rx.pop_front(), Some(3));
            assert_eq!(rx.pop_front(), None);
        }
    }
}
