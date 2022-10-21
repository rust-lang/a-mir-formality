use std::sync::Arc;

/// Our version of `From`. One twist: it is implemented for &T for all T.
pub trait FromTerm<T: Clone> {
    fn from_term(term: T) -> Self;
}

impl<T: Clone, U> FromTerm<&T> for U
where
    U: FromTerm<T>,
{
    fn from_term(term: &T) -> Self {
        U::from_term(T::clone(term))
    }
}

impl<T: Clone, U> FromTerm<Vec<T>> for Vec<U>
where
    U: FromTerm<T>,
{
    fn from_term(term: Vec<T>) -> Self {
        term.into_iter().map(|t| U::from_term(t)).collect()
    }
}

impl<T: Clone, U> FromTerm<&[T]> for Vec<U>
where
    U: FromTerm<T>,
{
    fn from_term(term: &[T]) -> Self {
        term.into_iter().map(|t| FromTerm::from_term(t)).collect()
    }
}

impl<T: Clone, U> FromTerm<Option<T>> for Option<U>
where
    U: FromTerm<T>,
{
    fn from_term(term: Option<T>) -> Self {
        term.map(|t| U::from_term(t))
    }
}

impl<T: Clone, U> FromTerm<Arc<T>> for Arc<U>
where
    U: FromTerm<T>,
{
    fn from_term(term: Arc<T>) -> Self {
        let term: &T = &term;
        Arc::new(FromTerm::from_term(term))
    }
}

/// Our version of `Into`
pub trait IntoTerm<T>: Clone {
    fn into_term(self) -> T;

    fn to_term(&self) -> T;
}

impl<T, U> IntoTerm<U> for T
where
    T: Clone,
    U: FromTerm<T>,
{
    fn into_term(self) -> U {
        U::from_term(self)
    }

    fn to_term(&self) -> U {
        <U as FromTerm<&T>>::from_term(self)
    }
}

#[macro_export]
macro_rules! self_from_term_impl {
    ($t:ty) => {
        impl FromTerm<$t> for $t {
            fn from_term(v: $t) -> $t {
                v
            }
        }
    };
}

#[macro_export]
macro_rules! from_term_impl {
    (impl FromTerm<$t:ident> for $e:ident) => {
        impl $crate::derive_links::FromTerm<$t> for $e {
            fn from_term(v: $t) -> $e {
                $e::$t(v)
            }
        }
    };

    (impl FromTerm<$t:ident> for $e:ident $(via $via:ident)+) => {
        impl $crate::derive_links::FromTerm<$t> for $e {
            fn from_term(v: $t) -> $e {
                $(
                    let v: $via = $crate::derive_links::FromTerm::from_term(v);
                )+
                <$e as $crate::derive_links::FromTerm<_>>::from_term(v)
            }
        }
    };
}
