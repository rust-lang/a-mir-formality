use std::sync::Arc;

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

/// Our version of `From`. One twist: it is implemented for &T for all T.
pub trait UpcastFrom<T: Clone> {
    fn from_term(term: T) -> Self;
}

impl<T: Clone, U> UpcastFrom<&T> for U
where
    T: Upcast<U>,
{
    fn from_term(term: &T) -> Self {
        T::upcast(T::clone(term))
    }
}

impl<T: Clone, U> UpcastFrom<Vec<T>> for Vec<U>
where
    T: Upcast<U>,
{
    fn from_term(term: Vec<T>) -> Self {
        term.into_iter().map(|t| T::upcast(t)).collect()
    }
}

impl<T: Clone, U> UpcastFrom<&[T]> for Vec<U>
where
    T: Upcast<U>,
{
    fn from_term(term: &[T]) -> Self {
        term.into_iter().map(|t| t.upcast()).collect()
    }
}

impl<T: Clone, U> UpcastFrom<Option<T>> for Option<U>
where
    T: Upcast<U>,
{
    fn from_term(term: Option<T>) -> Self {
        term.map(|t| t.upcast())
    }
}

impl<T: Clone, U> UpcastFrom<Arc<T>> for Arc<U>
where
    T: Upcast<U>,
{
    fn from_term(term: Arc<T>) -> Self {
        let term: &T = &term;
        Arc::new(term.to())
    }
}

/// Our version of `Into`
pub trait Upcast<T>: Clone {
    fn upcast(self) -> T;
}

impl<T, U> Upcast<U> for T
where
    T: Clone,
    U: UpcastFrom<T>,
{
    fn upcast(self) -> U {
        U::from_term(self)
    }
}

#[macro_export]
macro_rules! self_from_term_impl {
    ($t:ty) => {
        impl UpcastFrom<$t> for $t {
            fn from_term(v: $t) -> $t {
                v
            }
        }
    };
}

#[macro_export]
macro_rules! from_term_impl {
    (impl UpcastFrom<$t:ident> for $e:ident) => {
        impl $crate::derive_links::UpcastFrom<$t> for $e {
            fn from_term(v: $t) -> $e {
                $e::$t(v)
            }
        }
    };

    (impl UpcastFrom<$t:ident> for $e:ident $(via $via:ident)+) => {
        impl $crate::derive_links::UpcastFrom<$t> for $e {
            fn from_term(v: $t) -> $e {
                $(
                    let v: $via = $crate::derive_links::UpcastFrom::from_term(v);
                )+
                <$e as $crate::derive_links::UpcastFrom<_>>::from_term(v)
            }
        }
    };
}

impl<A, B, A1, B1> UpcastFrom<(A1, B1)> for (A, B)
where
    A1: Upcast<A>,
    B1: Upcast<B>,
{
    fn from_term(term: (A1, B1)) -> Self {
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
    fn from_term(term: (A1, B1, C1)) -> Self {
        let (a1, b1, c1) = term;
        (a1.upcast(), b1.upcast(), c1.upcast())
    }
}
