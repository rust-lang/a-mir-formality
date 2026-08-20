use std::sync::Arc;

use formality_core::{
    cast_impl, set, term, Cons, DowncastTo, Set, Upcast, UpcastFrom, Upcasted as _,
};

use crate::{grammar::WhereClause, prove::ToGoals};

use super::{Binder, Parameter, Predicate, TraitRef};

#[term($set)]
#[derive(Default)]
pub struct Goals {
    set: Set<Goal>,
}

impl Goals {
    pub fn t() -> Self {
        set![].upcast()
    }

    /// Goal(s) to prove `a` and `b` are equal (they must have equal length)
    pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Goals {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Predicate::equals(a, b))
            .collect()
    }

    /// Goal(s) to prove `a` and `b` are subtypes.
    ///
    /// FIXME(#220): This should take variance into account.
    pub fn all_sub(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Goals {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Predicate::sub(a, b))
            .collect()
    }

    /// Goal(s) to prove `a0: b` for all `a0` in `a`
    pub fn all_outlives(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Parameter>) -> Goals {
        let a: Vec<Parameter> = a.upcast();
        let b: Parameter = b.upcast();
        a.into_iter().map(|a| Predicate::outlives(a, &b)).collect()
    }

    /// Iterate over the goals in the set
    pub fn iter(&self) -> impl Iterator<Item = Goal> + use<'_> {
        self.into_iter()
    }
}

impl<'w> IntoIterator for &'w Goals {
    type Item = Goal;

    type IntoIter = Box<dyn Iterator<Item = Goal> + 'w>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.iter().cloned())
    }
}

impl IntoIterator for Goals {
    type Item = Goal;

    type IntoIter = Box<dyn Iterator<Item = Goal>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.into_iter())
    }
}

impl<I> FromIterator<I> for Goals
where
    I: Upcast<Goal>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        Goals {
            set: iter.into_iter().upcasted().collect(),
        }
    }
}

macro_rules! tuple_upcast {
    ($($name:ident),*) => {
        #[allow(non_snake_case)]
        impl<$($name,)*> UpcastFrom<($($name,)*)> for Goals
        where
            $($name: Upcast<Goals>,)*
        {
            fn upcast_from(($($name,)*): ($($name,)*)) -> Self {
                let c = None.into_iter();
                $(
                    let $name: Goals = $name.upcast();
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

impl UpcastFrom<Vec<WhereClause>> for Goals {
    fn upcast_from(clauses: Vec<WhereClause>) -> Self {
        clauses.to_goals()
    }
}

impl UpcastFrom<&[WhereClause]> for Goals {
    fn upcast_from(clauses: &[WhereClause]) -> Self {
        clauses.to_goals()
    }
}

impl DowncastTo<Cons<Goal, Goals>> for Goals {
    fn downcast_to(&self) -> Option<Cons<Goal, Goals>> {
        let Cons(goal, set) = self.set.downcast_to()?;
        Some(Cons(goal, set.upcast()))
    }
}

impl UpcastFrom<()> for Goals {
    fn upcast_from((): ()) -> Self {
        Goals::default()
    }
}

impl DowncastTo<()> for Goals {
    fn downcast_to(&self) -> Option<()> {
        if self.set.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

/// Something that can be proven: either an atomic [`Predicate`][], or a combinator
/// (`for<..>`, `if ..`) that builds a goal out of other goals.
///
/// Goals also serve as assumptions, i.e., the things we may take as true while proving.
#[term]
pub enum Goal {
    /// Means the predicate holds.
    #[cast]
    Predicate(Predicate),

    // Equivalent to `for<'a>` except that it can also express `for<T>` and so forth:
    // means `$v0` is true for any value of the bound variables (e.g., `'a` or `T`).
    #[grammar(for $v0)]
    ForAll(Arc<Binder<Goal>>),

    #[grammar(if $v0 $v1)]
    Implies(Goals, Arc<Goal>),
}

// ---

cast_impl!((TraitRef) <: (Predicate) <: (Goal));

impl UpcastFrom<Goal> for Goals {
    fn upcast_from(term: Goal) -> Self {
        Goals { set: set![term] }
    }
}

impl DowncastTo<Goal> for Goals {
    fn downcast_to(&self) -> Option<Goal> {
        if self.set.len() == 1 {
            self.set.iter().next().cloned()
        } else {
            None
        }
    }
}

cast_impl!((Predicate) <: (Goal) <: (Goals));
cast_impl!((TraitRef) <: (Goal) <: (Goals));
