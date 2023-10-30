use std::sync::Arc;

use crate::collections::Set;

pub trait To {
    fn to<T>(&self) -> T
    where
        Self: Upcast<T>;
}

impl<A> To for A {
    fn to<T>(&self) -> T
    where
        Self: Upcast<T>,
    {
        <&A as Upcast<T>>::upcast(self)
    }
}

/// Our version of `Into`. This is the trait you use in where clauses,
/// but you typically implement `UpcastFrom`.
pub trait Upcast<T>: Clone {
    fn upcast(self) -> T;
}

impl<T, U> Upcast<U> for T
where
    T: Clone,
    U: UpcastFrom<T>,
{
    fn upcast(self) -> U {
        U::upcast_from(self)
    }
}

/// Our version of `From`. One twist: it is implemented for &T for all T.
/// This is the trait you implement.
pub trait UpcastFrom<T: Clone> {
    fn upcast_from(term: T) -> Self;
}

/// This is the convenience trait whose method you should call to do a downcast.
pub trait Downcast: Sized {
    fn downcast<T>(&self) -> Option<T>
    where
        T: DowncastFrom<Self>;

    fn is_a<T>(&self) -> bool
    where
        T: DowncastFrom<Self>,
    {
        self.downcast::<T>().is_some()
    }
}

impl<U> Downcast for U {
    fn downcast<T>(&self) -> Option<T>
    where
        T: DowncastFrom<Self>,
    {
        T::downcast_from(self)
    }
}

/// Our version of "try-into". A "downcast" casts
/// from a more general type
/// (e.g., any Parameter) to a more specific type
/// (e.g., a type).
///
/// This is the trait that you prefer to implement,
/// but use `DowncastFrom` in where-clauses.
pub trait DowncastTo<T>: Sized {
    fn downcast_to(&self) -> Option<T>;
}

impl<T: Clone, U> DowncastTo<T> for &U
where
    T: DowncastFrom<U>,
{
    fn downcast_to(&self) -> Option<T> {
        T::downcast_from(self)
    }
}

impl<A, B> DowncastTo<Vec<B>> for Vec<A>
where
    B: DowncastFrom<A>,
{
    fn downcast_to(&self) -> Option<Vec<B>> {
        self.iter().map(|a| B::downcast_from(a)).collect()
    }
}

/// Our version of "try-from". A "downcast" casts
/// from a more general type
/// (e.g., any Parameter) to a more specific type
/// (e.g., a type). This returns an Option because
/// the value may not be an instance of the more
/// specific type.
pub trait DowncastFrom<T>: Sized {
    fn downcast_from(t: &T) -> Option<Self>;
}

impl<T, U> DowncastFrom<T> for U
where
    T: DowncastTo<U>,
{
    fn downcast_from(t: &T) -> Option<U> {
        t.downcast_to()
    }
}

impl<A, B> DowncastFrom<Set<A>> for Set<B>
where
    A: Ord,
    B: DowncastFrom<A> + Ord,
{
    fn downcast_from(t: &Set<A>) -> Option<Self> {
        t.iter().map(|a| B::downcast_from(a)).collect()
    }
}

impl<T, U> DowncastFrom<Option<U>> for Option<T>
where
    T: DowncastFrom<U>,
{
    fn downcast_from(u: &Option<U>) -> Option<Self> {
        match u {
            Some(u) => Some(Some(T::downcast_from(u)?)),
            None => None,
        }
    }
}

impl<T, U> DowncastFrom<Arc<U>> for Arc<T>
where
    T: DowncastFrom<U>,
{
    fn downcast_from(u: &Arc<U>) -> Option<Self> {
        let t: T = T::downcast_from(u)?;
        Some(Arc::new(t))
    }
}

impl<A, B, A1, B1> DowncastFrom<(A1, B1)> for (A, B)
where
    A: DowncastFrom<A1>,
    B: DowncastFrom<B1>,
{
    fn downcast_from(term: &(A1, B1)) -> Option<Self> {
        let (a1, b1) = term;
        let a = DowncastFrom::downcast_from(a1)?;
        let b = DowncastFrom::downcast_from(b1)?;
        Some((a, b))
    }
}

impl<A, B, C, A1, B1, C1> DowncastFrom<(A1, B1, C1)> for (A, B, C)
where
    A: DowncastFrom<A1>,
    B: DowncastFrom<B1>,
    C: DowncastFrom<C1>,
{
    fn downcast_from(term: &(A1, B1, C1)) -> Option<Self> {
        let (a1, b1, c1) = term;
        let a = DowncastFrom::downcast_from(a1)?;
        let b = DowncastFrom::downcast_from(b1)?;
        let c = DowncastFrom::downcast_from(c1)?;
        Some((a, b, c))
    }
}

impl<T: Clone, U> UpcastFrom<&T> for U
where
    T: Upcast<U>,
{
    fn upcast_from(term: &T) -> Self {
        T::upcast(T::clone(term))
    }
}

impl<T: Clone, U> UpcastFrom<Vec<T>> for Vec<U>
where
    T: Upcast<U>,
{
    fn upcast_from(term: Vec<T>) -> Self {
        term.into_iter().map(|t| T::upcast(t)).collect()
    }
}

impl<T: Clone, U> UpcastFrom<Set<T>> for Set<U>
where
    T: Upcast<U> + Ord,
    U: Ord,
{
    fn upcast_from(term: Set<T>) -> Self {
        term.into_iter().map(|t| T::upcast(t)).collect()
    }
}

impl<T: Clone, U> UpcastFrom<&[T]> for Vec<U>
where
    T: Upcast<U>,
{
    fn upcast_from(term: &[T]) -> Self {
        term.iter().map(|t| t.upcast()).collect()
    }
}

impl<T: Clone, U> UpcastFrom<Option<T>> for Option<U>
where
    T: Upcast<U>,
{
    fn upcast_from(term: Option<T>) -> Self {
        term.map(|t| t.upcast())
    }
}

impl DowncastTo<()> for () {
    fn downcast_to(&self) -> Option<()> {
        Some(())
    }
}

impl<T> UpcastFrom<()> for T
where
    T: Default,
{
    fn upcast_from((): ()) -> Self {
        Default::default()
    }
}

impl<T: Clone, U> UpcastFrom<Arc<T>> for Arc<U>
where
    T: Upcast<U>,
{
    fn upcast_from(term: Arc<T>) -> Self {
        let term: &T = &term;
        Arc::new(term.to())
    }
}

impl<A, B, A1, B1> UpcastFrom<(A1, B1)> for (A, B)
where
    A1: Upcast<A>,
    B1: Upcast<B>,
{
    fn upcast_from(term: (A1, B1)) -> Self {
        let (a1, b1) = term;
        (a1.upcast(), b1.upcast())
    }
}

impl<A, B, C, A1, B1, C1> UpcastFrom<(A1, B1, C1)> for (A, B, C)
where
    A1: Upcast<A>,
    B1: Upcast<B>,
    C1: Upcast<C>,
{
    fn upcast_from(term: (A1, B1, C1)) -> Self {
        let (a1, b1, c1) = term;
        (a1.upcast(), b1.upcast(), c1.upcast())
    }
}

impl<A, As, B, Bs, Z> UpcastFrom<(A, B)> for Vec<Z>
where
    A: IntoIterator<Item = As> + Clone,
    As: Upcast<Z>,
    B: IntoIterator<Item = Bs> + Clone,
    Bs: Upcast<Z>,
{
    fn upcast_from(term: (A, B)) -> Self {
        let (a, b) = term;
        let mut v: Self = vec![];
        v.extend(a.into_iter().map(|a| a.upcast()));
        v.extend(b.into_iter().map(|b| b.upcast()));
        v
    }
}

impl<A, B, C, Z> UpcastFrom<(A, B, C)> for Vec<Z>
where
    A: Upcast<Z>,
    B: Upcast<Z>,
    C: Upcast<Z>,
{
    fn upcast_from(term: (A, B, C)) -> Self {
        let (a, b, c) = term;
        vec![a.upcast(), b.upcast(), c.upcast()]
    }
}

#[macro_export]
macro_rules! cast_impl {
    ($e:ident :: $v:ident ($u:ty)) => {
        impl $crate::cast::UpcastFrom<$u> for $e {
            fn upcast_from(v: $u) -> $e {
                $e::$v(v)
            }
        }

        impl $crate::cast::DowncastTo<$u> for $e {
            fn downcast_to(&self) -> Option<$u> {
                match self {
                    $e::$v(u) => Some(Clone::clone(u)),
                    _ => None,
                }
            }
        }
    };

    (impl($($p:tt)*) $e:ident ($($ep:tt)*) :: $v:ident ($u:ty)) => {
        impl<$($p)*> $crate::cast::UpcastFrom<$u> for $e<$($ep)*> {
            fn upcast_from(v: $u) -> $e<$($ep)*> {
                $e::$v(v)
            }
        }

        impl<$($p)*> $crate::cast::DowncastTo<$u> for $e<$($ep)*> {
            fn downcast_to(&self) -> Option<$u> {
                match self {
                    $e::$v(u) => Some(Clone::clone(u)),
                    _ => None,
                }
            }
        }
    };

    (impl($($p:tt)*) $t:ty) => {
        impl<$($p)*> $crate::cast::UpcastFrom<$t> for $t {
            fn upcast_from(v: $t) -> $t {
                v
            }
        }

        impl<$($p)*> $crate::cast::DowncastTo<$t> for $t {
            fn downcast_to(&self) -> Option<$t> {
                Some(Self::clone(self))
            }
        }
    };

    ($t:ty) => {
        impl $crate::cast::UpcastFrom<$t> for $t {
            fn upcast_from(v: $t) -> $t {
                v
            }
        }

        impl $crate::cast::DowncastTo<$t> for $t {
            fn downcast_to(&self) -> Option<$t> {
                Some(Self::clone(self))
            }
        }
    };

    ($(impl($($p:tt)*))? ($bot:ty) <: ($($mid:ty),*) <: ($top:ty)) => {
        impl$(<$($p)*>)? $crate::cast::UpcastFrom<$bot> for $top {
            fn upcast_from(v: $bot) -> $top {
                $(
                    let v: $mid = $crate::cast::Upcast::upcast(v);
                )*
                $crate::cast::Upcast::upcast(v)
            }
        }

        impl$(<$($p)*>)? $crate::cast::DowncastTo<$bot> for $top {
            fn downcast_to(&self) -> Option<$bot> {
                let v: &$top = self;
                $(
                    let v: &$mid = &$crate::cast::DowncastFrom::downcast_from(v)?;
                )*
                $crate::cast::DowncastFrom::downcast_from(v)
            }
        }
    };
}

cast_impl!(usize);
cast_impl!(u32);
cast_impl!(String);

impl UpcastFrom<&str> for String {
    fn upcast_from(term: &str) -> Self {
        term.into()
    }
}

pub trait Upcasted<'a, T> {
    fn upcasted(self) -> Box<dyn Iterator<Item = T> + 'a>;
}

impl<'a, T, I> Upcasted<'a, T> for I
where
    I: IntoIterator + 'a,
    I::Item: Upcast<T>,
{
    fn upcasted(self) -> Box<dyn Iterator<Item = T> + 'a> {
        Box::new(self.into_iter().map(|e| e.upcast()))
    }
}

pub trait Downcasted<'a>: IntoIterator {
    fn downcasted<T>(self) -> Box<dyn Iterator<Item = T> + 'a>
    where
        T: DowncastFrom<Self::Item>;
}

impl<'a, I> Downcasted<'a> for I
where
    I: IntoIterator + 'a,
{
    fn downcasted<T>(self) -> Box<dyn Iterator<Item = T> + 'a>
    where
        T: DowncastFrom<I::Item>,
    {
        Box::new(self.into_iter().filter_map(|e| T::downcast_from(&e)))
    }
}
