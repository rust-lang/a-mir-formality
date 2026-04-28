use std::sync::Arc;

use formality_core::{
    cast_impl, set, term, Cons, DowncastTo, Set, Upcast, UpcastFrom, Upcasted as _,
};

use crate::{grammar::WhereClause, prove::ToWcs};

use super::{Binder, Parameter, Predicate, Relation, TraitRef};

#[term($set)]
#[derive(Default)]
pub struct Wcs {
    set: Set<Wc>,
}

impl Wcs {
    pub fn t() -> Self {
        set![].upcast()
    }

    /// Goal(s) to prove `a` and `b` are equal (they must have equal length)
    pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Relation::equals(a, b))
            .collect()
    }

    /// Goal(s) to prove `a` and `b` are subtypes.
    ///
    /// FIXME(#220): This should take variance into account.
    pub fn all_sub(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Relation::sub(a, b))
            .collect()
    }

    /// Goal(s) to prove `a0: b` for all `a0` in `a`
    pub fn all_outlives(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Parameter>) -> Wcs {
        let a: Vec<Parameter> = a.upcast();
        let b: Parameter = b.upcast();
        a.into_iter().map(|a| Relation::outlives(a, &b)).collect()
    }

    /// Iterate over where-clauses
    pub fn iter(&self) -> impl Iterator<Item = Wc> + use<'_> {
        self.into_iter()
    }
}

impl<'w> IntoIterator for &'w Wcs {
    type Item = Wc;

    type IntoIter = Box<dyn Iterator<Item = Wc> + 'w>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.iter().cloned())
    }
}

impl IntoIterator for Wcs {
    type Item = Wc;

    type IntoIter = Box<dyn Iterator<Item = Wc>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.into_iter())
    }
}

impl<I> FromIterator<I> for Wcs
where
    I: Upcast<Wc>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        Wcs {
            set: iter.into_iter().upcasted().collect(),
        }
    }
}

macro_rules! tuple_upcast {
    ($($name:ident),*) => {
        #[allow(non_snake_case)]
        impl<$($name,)*> UpcastFrom<($($name,)*)> for Wcs
        where
            $($name: Upcast<Wcs>,)*
        {
            fn upcast_from(($($name,)*): ($($name,)*)) -> Self {
                let c = None.into_iter();
                $(
                    let $name: Wcs = $name.upcast();
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

impl UpcastFrom<Vec<WhereClause>> for Wcs {
    fn upcast_from(clauses: Vec<WhereClause>) -> Self {
        clauses.to_wcs()
    }
}

impl UpcastFrom<&[WhereClause]> for Wcs {
    fn upcast_from(clauses: &[WhereClause]) -> Self {
        clauses.to_wcs()
    }
}

impl DowncastTo<Cons<Wc, Wcs>> for Wcs {
    fn downcast_to(&self) -> Option<Cons<Wc, Wcs>> {
        let Cons(wc, set) = self.set.downcast_to()?;
        Some(Cons(wc, set.upcast()))
    }
}

impl UpcastFrom<()> for Wcs {
    fn upcast_from((): ()) -> Self {
        Wcs::default()
    }
}

impl DowncastTo<()> for Wcs {
    fn downcast_to(&self) -> Option<()> {
        if self.set.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

#[term]
pub enum Wc {
    /// Means the built-in relation holds.
    #[cast]
    Relation(Relation),

    /// Means the predicate holds.
    #[cast]
    Predicate(Predicate),

    // Equivalent to `for<'a>` except that it can also express `for<T>` and so forth:
    // means `$v0` is true for any value of the bound variables (e.g., `'a` or `T`).
    #[grammar(for $v0)]
    ForAll(Arc<Binder<Wc>>),

    #[grammar(if $v0 $v1)]
    Implies(Wcs, Arc<Wc>),
}

/// Temporary alias for migration -- allows `WcData::Variant` to still compile.
pub type WcData = Wc;

// ---

cast_impl!((TraitRef) <: (Predicate) <: (Wc));

impl UpcastFrom<Wc> for Wcs {
    fn upcast_from(term: Wc) -> Self {
        Wcs { set: set![term] }
    }
}

impl DowncastTo<Wc> for Wcs {
    fn downcast_to(&self) -> Option<Wc> {
        if self.set.len() == 1 {
            self.set.iter().next().cloned()
        } else {
            None
        }
    }
}

cast_impl!((Relation) <: (Wc) <: (Wcs));
cast_impl!((Predicate) <: (Wc) <: (Wcs));
cast_impl!((TraitRef) <: (Wc) <: (Wcs));
