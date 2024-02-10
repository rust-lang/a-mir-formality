//! We use btree maps and sets, and we rely on the ordering
//! properties that they ensure.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    cast::{DowncastTo, Upcast, UpcastFrom, Upcasted},
    Downcast,
};

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

pub trait SetExt<T>: Sized {
    fn split_first(self) -> Option<(T, Set<T>)>;

    fn union_with(self, other: impl Upcast<Set<T>>) -> Set<T>;

    fn with_element(self, other: impl Upcast<T>) -> Set<T>;

    fn without_element(self, other: impl Upcast<T>) -> Set<T>;

    fn split_nth(&self, i: usize) -> Option<(T, Set<T>)>;
}

impl<T: Ord + Clone> SetExt<T> for &Set<T> {
    fn split_first(self) -> Option<(T, Set<T>)> {
        self.clone().split_first()
    }

    fn union_with(self, other: impl Upcast<Set<T>>) -> Set<T> {
        self.clone().union_with(other)
    }

    fn with_element(self, other: impl Upcast<T>) -> Set<T> {
        self.clone().with_element(other)
    }

    fn without_element(self, other: impl Upcast<T>) -> Set<T> {
        self.clone().without_element(other)
    }

    fn split_nth(&self, i: usize) -> Option<(T, Set<T>)> {
        <Set<T>>::split_nth(self, i)
    }
}

impl<T: Ord + Clone> SetExt<T> for Set<T> {
    fn split_first(self) -> Option<(T, Set<T>)> {
        let mut iter = self.into_iter();
        let e = iter.next()?;
        let c = iter.collect();
        Some((e, c))
    }

    fn split_nth(&self, i: usize) -> Option<(T, Set<T>)> {
        let mut s = self.clone();
        let item = self.iter().nth(i)?;
        let item = s.take(item).unwrap();
        Some((item, s))
    }

    fn union_with(mut self, other: impl Upcast<Set<T>>) -> Set<T> {
        let other: Set<T> = other.upcast();
        for item in other {
            self.insert(item);
        }
        self
    }

    fn with_element(mut self, other: impl Upcast<T>) -> Set<T> {
        self.insert(other.upcast());
        self
    }

    fn without_element(mut self, other: impl Upcast<T>) -> Set<T> {
        self.remove(&other.upcast());
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

impl<A, T> DowncastTo<(A,)> for Set<T>
where
    T: DowncastTo<A> + Ord,
{
    fn downcast_to(&self) -> Option<(A,)> {
        if self.len() == 1 {
            let a: A = self.first().unwrap().downcast()?;
            Some((a,))
        } else {
            None
        }
    }
}

impl<A, T> UpcastFrom<(A,)> for Set<T>
where
    A: Clone + Upcast<T>,
    T: Ord + Clone,
{
    fn upcast_from(term: (A,)) -> Self {
        let (a,) = term;
        set![a.upcast()]
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

impl<A, T> DowncastTo<(A,)> for Vec<T>
where
    T: DowncastTo<A>,
{
    fn downcast_to(&self) -> Option<(A,)> {
        if self.len() == 1 {
            let a: A = self.first().unwrap().downcast()?;
            Some((a,))
        } else {
            None
        }
    }
}

impl<A, T> UpcastFrom<(A,)> for Vec<T>
where
    A: Clone + Upcast<T>,
    T: Clone,
{
    fn upcast_from(term: (A,)) -> Self {
        let (a,) = term;
        vec![a.upcast()]
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
/// You can upcast/downcast a `Vec` or `Set` to `Cons(head, tail)`
/// where `head` will be the first item in the collection
/// and tail will be a collection with the remaining items.
/// Both can also be upcast/downcast to `()` which matches an empty collection.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cons<T, C>(pub T, pub C);

impl<T, U, V> UpcastFrom<Cons<U, V>> for Set<T>
where
    T: Ord + Clone,
    U: Upcast<T>,
    V: Upcast<Set<T>>,
{
    fn upcast_from(term: Cons<U, V>) -> Self {
        let Cons(elem, set) = term;
        let mut set: Set<T> = set.upcast();
        set.insert(elem.upcast());
        set
    }
}

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

impl<T, U, V> UpcastFrom<Cons<U, V>> for Vec<T>
where
    U: Upcast<T>,
    V: Upcast<Vec<T>>,
{
    fn upcast_from(term: Cons<U, V>) -> Self {
        let Cons(elem, vec) = term;
        let mut vec: Vec<T> = vec.upcast();
        vec.insert(0, elem.upcast());
        vec
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
