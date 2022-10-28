//! We use btree maps and sets, and we rely on the ordering
//! properties that they ensure.

use std::collections::{BTreeMap, BTreeSet};

pub type Map<K, V> = BTreeMap<K, V>;
pub type Set<E> = BTreeSet<E>;

#[macro_export]
macro_rules! set {
    () => {
        Set::new()
    };

    ($($e:expr),*) => {
        $crate::seq![$($e),*].into_iter().collect::<Set<_>>()
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
