use std::sync::Arc;

use formality_core::{
    cast_impl, set, term, Cons, DowncastTo, Set, Upcast, UpcastFrom, Upcasted as _,
};

use crate::{grammar::WhereClause, prove::ToPredicates};

use super::{AtomicPredicate, Binder, Parameter, TraitRef};

#[term($set)]
#[derive(Default)]
pub struct Predicates {
    set: Set<Predicate>,
}

impl Predicates {
    pub fn t() -> Self {
        set![].upcast()
    }

    /// Goal(s) to prove `a` and `b` are equal (they must have equal length)
    pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Predicates {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| AtomicPredicate::equals(a, b))
            .collect()
    }

    /// Goal(s) to prove `a` and `b` are subtypes.
    ///
    /// FIXME(#220): This should take variance into account.
    pub fn all_sub(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Predicates {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| AtomicPredicate::sub(a, b))
            .collect()
    }

    /// Goal(s) to prove `a0: b` for all `a0` in `a`
    pub fn all_outlives(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Parameter>) -> Predicates {
        let a: Vec<Parameter> = a.upcast();
        let b: Parameter = b.upcast();
        a.into_iter()
            .map(|a| AtomicPredicate::outlives(a, &b))
            .collect()
    }

    /// Iterate over where-clauses
    pub fn iter(&self) -> impl Iterator<Item = Predicate> + use<'_> {
        self.into_iter()
    }
}

impl<'w> IntoIterator for &'w Predicates {
    type Item = Predicate;

    type IntoIter = Box<dyn Iterator<Item = Predicate> + 'w>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.iter().cloned())
    }
}

impl IntoIterator for Predicates {
    type Item = Predicate;

    type IntoIter = Box<dyn Iterator<Item = Predicate>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.into_iter())
    }
}

impl<I> FromIterator<I> for Predicates
where
    I: Upcast<Predicate>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        Predicates {
            set: iter.into_iter().upcasted().collect(),
        }
    }
}

macro_rules! tuple_upcast {
    ($($name:ident),*) => {
        #[allow(non_snake_case)]
        impl<$($name,)*> UpcastFrom<($($name,)*)> for Predicates
        where
            $($name: Upcast<Predicates>,)*
        {
            fn upcast_from(($($name,)*): ($($name,)*)) -> Self {
                let c = None.into_iter();
                $(
                    let $name: Predicates = $name.upcast();
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

impl UpcastFrom<Vec<WhereClause>> for Predicates {
    fn upcast_from(clauses: Vec<WhereClause>) -> Self {
        clauses.to_predicates()
    }
}

impl UpcastFrom<&[WhereClause]> for Predicates {
    fn upcast_from(clauses: &[WhereClause]) -> Self {
        clauses.to_predicates()
    }
}

impl DowncastTo<Cons<Predicate, Predicates>> for Predicates {
    fn downcast_to(&self) -> Option<Cons<Predicate, Predicates>> {
        let Cons(wc, set) = self.set.downcast_to()?;
        Some(Cons(wc, set.upcast()))
    }
}

impl UpcastFrom<()> for Predicates {
    fn upcast_from((): ()) -> Self {
        Predicates::default()
    }
}

impl DowncastTo<()> for Predicates {
    fn downcast_to(&self) -> Option<()> {
        if self.set.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

/// The lowered form of a [`WhereClause`](crate::grammar::WhereClause), produced by
/// [`ToPredicates`](crate::prove::ToPredicates). Used both for goals to prove and for
/// assumptions. A predicate is either an [`AtomicPredicate`][] or a combinator
/// (`ForAll`, `Implies`) over other predicates.
#[term]
pub enum Predicate {
    /// Means the predicate holds.
    #[cast]
    AtomicPredicate(AtomicPredicate),

    // Equivalent to `for<'a>` except that it can also express `for<T>` and so forth:
    // means `$v0` is true for any value of the bound variables (e.g., `'a` or `T`).
    #[grammar(for $v0)]
    ForAll(Arc<Binder<Predicate>>),

    #[grammar(if $v0 $v1)]
    Implies(Predicates, Arc<Predicate>),
}

// ---

cast_impl!((TraitRef) <: (AtomicPredicate) <: (Predicate));

impl UpcastFrom<Predicate> for Predicates {
    fn upcast_from(term: Predicate) -> Self {
        Predicates { set: set![term] }
    }
}

impl DowncastTo<Predicate> for Predicates {
    fn downcast_to(&self) -> Option<Predicate> {
        if self.set.len() == 1 {
            self.set.iter().next().cloned()
        } else {
            None
        }
    }
}

cast_impl!((AtomicPredicate) <: (Predicate) <: (Predicates));
cast_impl!((TraitRef) <: (Predicate) <: (Predicates));
