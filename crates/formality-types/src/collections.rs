//! We use btree maps and sets, and we rely on the ordering
//! properties that they ensure.

use std::collections::{BTreeMap, BTreeSet};

pub type Map<K, V> = BTreeMap<K, V>;
pub type Set<E> = BTreeSet<E>;

#[macro_export]
macro_rules! set {
    () => {
        $crate::collections::Set::new()
    };

    ($($t:tt)*) => {
        $crate::seq![$($t)*].into_iter().collect::<$crate::collections::Set<_>>()
    };
}

/// Replacement for the `vec` macro that supports `..foo` notation to flatten
/// vectors.
#[macro_export]
macro_rules! seq {
    (@elem($iter:ident) .. $e:expr) => {
        let $iter = $iter.chain($e);
    };

    (@elem($iter:ident) .. $e:expr, $($t:tt)*) => {
        let $iter = $iter.chain($e);
        $crate::seq!(@elem($iter) $($t)*);
    };

    (@elem($iter:ident) $e:expr, $($t:tt)*) => {
        let $iter = $iter.chain(std::iter::once($e));
        $crate::seq!(@elem($iter) $($t)*);
    };

    (@elem($iter:ident)) => {
    };

    (@elem($iter:ident) $($t:tt)*) => {
        let $iter = $iter.chain(std::iter::once($($t)*));
    };

    () => {
        vec![]
    };

    ($($t:tt)*) => {
        {
            let iter = std::iter::empty();
            $crate::seq!(@elem(iter) $($t)*);
            iter.collect::<Vec<_>>()
        }
    };

}

pub trait SetExt<T>: Sized {
    fn split_first(self) -> Option<(T, Self)>;

    fn union_with(&self, other: Self) -> Self;
}

impl<T: Ord + Clone> SetExt<T> for Set<T> {
    fn split_first(mut self) -> Option<(T, Set<T>)> {
        if let Some(e) = self.pop_first() {
            Some((e, self))
        } else {
            None
        }
    }

    fn union_with(&self, other: Self) -> Self {
        self.iter().cloned().chain(other).collect()
    }
}
