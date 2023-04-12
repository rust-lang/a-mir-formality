//! We use btree maps and sets, and we rely on the ordering
//! properties that they ensure.

use std::collections::{BTreeMap, BTreeSet};

use crate::cast::{Downcast, DowncastFrom, DowncastTo, Upcast, UpcastFrom};

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

    fn union_with(self, other: Self) -> Self;

    fn plus(self, other: T) -> Self;
}

impl<T: Ord + Clone> SetExt<T> for Set<T> {
    fn split_first(self) -> Option<(T, Set<T>)> {
        let mut iter = self.into_iter();
        if let Some(e) = iter.next() {
            Some((e, iter.collect()))
        } else {
            None
        }
    }

    fn union_with(mut self, other: Self) -> Self {
        for item in other {
            self.insert(item);
        }
        self
    }

    fn plus(mut self, other: T) -> Self {
        self.insert(other);
        self
    }
}

impl<T> DowncastTo<()> for Set<T> {
    fn downcast_to(&self) -> Option<()> {
        if self.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

impl<T> DowncastTo<()> for Vec<T> {
    fn downcast_to(&self) -> Option<()> {
        if self.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

macro_rules! tuple_upcast {
    ($($name:ident),*) => {
        #[allow(non_snake_case)]
        impl<$($name,)* T> UpcastFrom<($($name,)*)> for Set<T>
        where
            $($name: Upcast<Set<T>>,)*
            T: Ord + Clone,
        {
            fn upcast_from(($($name,)*): ($($name,)*)) -> Self {
                let c = None.into_iter();
                $(
                    let $name: Set<T> = $name.upcast();
                    let c = c.chain($name);
                )*
                c.collect()
            }
        }
    }
}

tuple_upcast!(A, B);
tuple_upcast!(A, B, C);
tuple_upcast!(A, B, C, D);

impl<A, T> DowncastTo<(A, Set<T>)> for Set<T>
where
    A: DowncastFrom<T>,
    T: Ord + Clone,
{
    fn downcast_to(&self) -> Option<(A, Set<T>)> {
        if self.is_empty() {
            None
        } else {
            let r = self.clone();
            let (a, bs) = r.split_first().unwrap();
            let a: A = a.downcast()?;
            Some((a, bs))
        }
    }
}

impl<A, T> DowncastTo<(A, Vec<T>)> for Vec<T>
where
    A: DowncastFrom<T>,
    T: Ord + Clone,
{
    fn downcast_to(&self) -> Option<(A, Vec<T>)> {
        if self.is_empty() {
            None
        } else {
            let r = self.clone();
            let (a, bs) = r.split_first().unwrap();
            let a: A = a.downcast()?;
            Some((a, bs.to_vec()))
        }
    }
}

pub trait Deduplicate {
    fn deduplicate(self) -> Self;
    fn remove_dups(&mut self);
}

impl<T> Deduplicate for Vec<T>
where
    T: Ord + Clone,
{
    /// Remove duplicates of `v`
    fn deduplicate(mut self) -> Vec<T> {
        self.remove_dups();
        self
    }

    fn remove_dups(&mut self) {
        let mut s = Set::default();
        self.retain(|e| s.insert(e.clone()));
    }
}
