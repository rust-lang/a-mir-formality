use std::sync::Arc;

use anyhow::bail;

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

    fn downcast_err<T>(&self) -> anyhow::Result<T>
    where
        T: DowncastFrom<Self>,
    {
        let Some(value) = self.downcast() else {
            bail!(
                "downcasting from `{}` to `{}` failed",
                std::any::type_name::<Self>(),
                std::any::type_name::<T>()
            )
        };

        Ok(value)
    }

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

impl<A, B, T> DowncastFrom<Vec<T>> for (A, B)
where
    A: DowncastFrom<T>,
    B: DowncastFrom<T>,
{
    fn downcast_from(term: &Vec<T>) -> Option<Self> {
        if term.len() != 2 {
            return None;
        }

        let a = &term[0];
        let b = &term[1];
        Some((a.downcast()?, b.downcast()?))
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

impl<T: Clone, U, E> UpcastFrom<Result<T, E>> for Result<U, E>
where
    T: Upcast<U>,
    E: Clone,
{
    fn upcast_from(term: Result<T, E>) -> Self {
        term.map(|t| t.upcast())
    }
}

impl DowncastTo<()> for () {
    fn downcast_to(&self) -> Option<()> {
        Some(())
    }
}

impl UpcastFrom<()> for () {
    fn upcast_from((): ()) -> Self {}
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

macro_rules! tuple_impl {
    ($($from:ident),*; $($to:ident),*) => {
        impl<$($from,)* $($to,)*> UpcastFrom<($($from,)*)> for ($($to,)*)
        where
            $($from: Upcast<$to>,)*
        {
            #[allow(non_snake_case)]
            fn upcast_from(term: ($($from,)*)) -> Self {
                let ($($from,)*) = term;
                ($($from.upcast(),)*)
            }
        }
    };
}

tuple_impl!(A1, B1; A, B);
tuple_impl!(A1, B1, C1; A, B, C);
tuple_impl!(A1, B1, C1, D1; A, B, C, D);
tuple_impl!(A1, B1, C1, D1, E1; A, B, C, D, E);
tuple_impl!(A1, B1, C1, D1, E1, F1; A, B, C, D, E, F);
tuple_impl!(A1, B1, C1, D1, E1, F1, G1; A, B, C, D, E, F, G);

#[macro_export]
macro_rules! cast_impl {
    ($e:ident :: $v:ident ($u:ty)) => {
        impl $crate::UpcastFrom<$u> for $e {
            fn upcast_from(v: $u) -> $e {
                $e::$v(v)
            }
        }

        impl $crate::DowncastTo<$u> for $e {
            fn downcast_to(&self) -> Option<$u> {
                match self {
                    $e::$v(u) => Some(Clone::clone(u)),
                    _ => None,
                }
            }
        }
    };

    (impl($($p:tt)*) $e:ident ($($ep:tt)*) :: $v:ident ($u:ty)) => {
        impl<$($p)*> $crate::UpcastFrom<$u> for $e<$($ep)*> {
            fn upcast_from(v: $u) -> $e<$($ep)*> {
                $e::$v(v)
            }
        }

        impl<$($p)*> $crate::DowncastTo<$u> for $e<$($ep)*> {
            fn downcast_to(&self) -> Option<$u> {
                match self {
                    $e::$v(u) => Some(Clone::clone(u)),
                    _ => None,
                }
            }
        }
    };

    (impl($($p:tt)*) $t:ty) => {
        impl<$($p)*> $crate::UpcastFrom<$t> for $t {
            fn upcast_from(v: $t) -> $t {
                v
            }
        }

        impl<$($p)*> $crate::DowncastTo<$t> for $t {
            fn downcast_to(&self) -> Option<$t> {
                Some(Self::clone(self))
            }
        }
    };

    ($t:ty) => {
        impl $crate::UpcastFrom<$t> for $t {
            fn upcast_from(v: $t) -> $t {
                v
            }
        }

        impl $crate::DowncastTo<$t> for $t {
            fn downcast_to(&self) -> Option<$t> {
                Some(Self::clone(self))
            }
        }
    };

    ($(impl($($p:tt)*))? ($bot:ty) <: ($($mid:ty),*) <: ($top:ty)) => {
        impl$(<$($p)*>)? $crate::UpcastFrom<$bot> for $top {
            fn upcast_from(v: $bot) -> $top {
                $(
                    let v: $mid = $crate::Upcast::upcast(v);
                )*
                $crate::Upcast::upcast(v)
            }
        }

        impl$(<$($p)*>)? $crate::DowncastTo<$bot> for $top {
            fn downcast_to(&self) -> Option<$bot> {
                let v: &$top = self;
                $(
                    let v: &$mid = &$crate::DowncastFrom::downcast_from(v)?;
                )*
                $crate::DowncastFrom::downcast_from(v)
            }
        }
    };
}

cast_impl!(bool);
cast_impl!(usize);
cast_impl!(u8);
cast_impl!(u16);
cast_impl!(u32);
cast_impl!(u64);
cast_impl!(i8);
cast_impl!(i16);
cast_impl!(i32);
cast_impl!(i64);
cast_impl!(isize);
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
