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
        vec![$($e),*].into_iter().collect::<Set<_>>()
    };
}
