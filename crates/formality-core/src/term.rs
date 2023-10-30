use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::{
    binder::Binder,
    cast::{DowncastFrom, Upcast},
    collections::Set,
    fold::Fold,
    language::Language,
    parse::Parse,
};

pub trait Term<L: Language>:
    Clone
    + Fold<L>
    + Parse<L>
    + Ord
    + Eq
    + Hash
    + Debug
    + Upcast<Self>
    + DowncastFrom<Self>
    + 'static
    + Sized
{
}

impl<L: Language, T: Term<L>> Term<L> for Vec<T> {}

impl<L: Language, T: Term<L>> Term<L> for Set<T> {}

impl<L: Language, T: Term<L>> Term<L> for Option<T> {}

impl<L: Language, T: Term<L>> Term<L> for Arc<T> {}

impl<L: Language> Term<L> for usize {}

impl<L: Language> Term<L> for u32 {}

impl<L: Language, A: Term<L>, B: Term<L>> Term<L> for (A, B) {}

impl<L: Language, T: Term<L>> Term<L> for Binder<L, T> {}

impl<L: Language> Term<L> for () {}
