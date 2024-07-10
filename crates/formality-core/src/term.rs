use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::{
    binder::CoreBinder,
    cast::{DowncastFrom, Upcast},
    collections::Set,
    fold::CoreFold,
    fuzz::Fuzzable,
    language::Language,
    parse::CoreParse,
};

pub trait CoreTerm<L: Language>:
    Clone
    + Ord
    + Eq
    + Hash
    + Debug
    + CoreFold<L>
    + CoreParse<L>
    + Fuzzable<L>
    + Upcast<Self>
    + DowncastFrom<Self>
    + 'static
    + Sized
{
}

impl<L: Language, T: CoreTerm<L>> CoreTerm<L> for Vec<T> {}

impl<L: Language, T: CoreTerm<L>> CoreTerm<L> for Set<T> {}

impl<L: Language, T: CoreTerm<L>> CoreTerm<L> for Option<T> {}

impl<L: Language, T: CoreTerm<L>> CoreTerm<L> for Arc<T> {}

impl<L: Language> CoreTerm<L> for usize {}

impl<L: Language> CoreTerm<L> for u32 {}

impl<L: Language, A: CoreTerm<L>, B: CoreTerm<L>> CoreTerm<L> for (A, B) {}

impl<L: Language, T: CoreTerm<L>> CoreTerm<L> for CoreBinder<L, T> {}

impl<L: Language> CoreTerm<L> for () {}
