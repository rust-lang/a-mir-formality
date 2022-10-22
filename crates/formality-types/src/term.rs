use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::{
    cast::{DowncastFrom, Upcast},
    fold::Fold,
    grammar::{Lt, Ty},
    parse::Parse,
};

pub trait Term:
    Clone + Fold + Parse + Ord + Eq + Hash + Debug + Upcast<Self> + DowncastFrom<Self> + 'static + Sized
{
}

impl<T: Term> Term for Vec<T> {}

impl<T: Term> Term for Option<T> {}

impl<T: Term> Term for Arc<T> {}

impl Term for Ty {}

impl Term for Lt {}

impl Term for usize {}

impl Term for u32 {}

impl<A: Term, B: Term> Term for (A, B) {}

impl<A: Term, B: Term, C: Term> Term for (A, B, C) {}
