//! We use btree maps and sets, and we rely on the ordering
//! properties that they ensure.

use std::collections::{BTreeMap, BTreeSet};

use crate::cast::{DowncastTo, Upcast, UpcastFrom, Upcasted};

pub type Map<K, V> = BTreeMap<K, V>;
pub type Set<E> = BTreeSet<E>;

#[macro_export]
macro_rules! set {
    () => {
        $crate::Set::new()
    };

    ($($t:tt)*) => {
        $crate::seq![$($t)*].into_iter().collect::<$crate::Set<_>>()
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

pub trait VecExt<T>: Sized {
    fn with_pushed(self, other: T) -> Self;
}

impl<T: Ord + Clone> VecExt<T> for Vec<T> {
    fn with_pushed(mut self, other: T) -> Self {
        self.push(other);
        self
    }
}

pub trait SetExt<T>: Sized {
    fn split_first(self) -> Option<(T, Self)>;

    fn union_with(self, other: Self) -> Self;

    fn plus(self, other: T) -> Self;
}

impl<T: Ord + Clone> SetExt<T> for Set<T> {
    fn split_first(self) -> Option<(T, Set<T>)> {
        let mut iter = self.into_iter();
        let e = iter.next()?;
        let c = iter.collect();
        Some((e, c))
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
    ($coll:ident : $($name:ident),*) => {
        /// Upcast from a tuple of iterable things into a collection.
        /// Downcasting doesn't work, because how would we know how many
        /// things to put in each collection? But see `Cons` below.
        #[allow(non_snake_case)]
        impl<$($name,)* T> UpcastFrom<($($name,)*)> for $coll<T>
        where
            $($name: IntoIterator + Clone,)*
            $(<$name as IntoIterator>::Item: Upcast<T>,)*
            T: Ord + Clone,
        {
            fn upcast_from(($($name,)*): ($($name,)*)) -> Self {
                let c = None.into_iter();
                $(
                    let c = c.chain($name.into_iter().upcasted());
                )*
                c.collect()
            }
        }
    }
}

tuple_upcast!(Set: );
tuple_upcast!(Set: A, B);
tuple_upcast!(Set: A, B, C);
tuple_upcast!(Set: A, B, C, D);

tuple_upcast!(Vec: );
tuple_upcast!(Vec: A, B);
tuple_upcast!(Vec: A, B, C);
tuple_upcast!(Vec: A, B, C, D);

/// This type exists to be used in judgment functions.
/// You can downcast a `Vec` or `Set` to `Cons(head, tail)`
/// where `head` will be the first item in the collection
/// and tail will be a collection with the remaining items.
/// Both can also be downcast to `()` which matches an empty collection.
pub struct Cons<T, C>(pub T, pub C);

impl<T> DowncastTo<Cons<T, Set<T>>> for Set<T>
where
    T: Ord + Clone,
{
    fn downcast_to(&self) -> Option<Cons<T, Set<T>>> {
        if self.is_empty() {
            None
        } else {
            let r = self.clone();
            let (a, bs) = r.split_first().unwrap();
            Some(Cons(a, bs))
        }
    }
}

impl<T> DowncastTo<Cons<T, Vec<T>>> for Vec<T>
where
    T: Ord + Clone,
{
    fn downcast_to(&self) -> Option<Cons<T, Vec<T>>> {
        let (a, bs) = self.split_first()?;
        Some(Cons(a.clone(), bs.to_vec()))
    }
}

pub trait Deduplicate {
    fn deduplicate(self) -> Self;
}

impl<T> Deduplicate for Vec<T>
where
    T: Ord + Clone,
{
    /// Remove duplicates of `v`
    fn deduplicate(mut self) -> Vec<T> {
        let mut s = Set::default();
        self.retain(|e| s.insert(e.clone()));
        self
    }
}
