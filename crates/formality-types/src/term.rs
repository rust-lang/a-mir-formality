use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::{
    fold::Fold,
    grammar::{Lt, Ty},
    parse::Parse,
    from_into_term::FromTerm,
};

pub trait Term: Clone + Fold + Parse + Ord + Eq + Hash + Debug + FromTerm<Self> {}

impl<T: Term> Term for Vec<T> {}

impl<T: Term> Term for Option<T> {}

impl<T: Term> Term for Arc<T> {}

impl Term for Ty {}
crate::self_from_term_impl!(Ty);

impl Term for Lt {}
crate::self_from_term_impl!(Lt);

impl Term for usize {}
crate::self_from_term_impl!(usize);

impl Term for u32 {}
crate::self_from_term_impl!(u32);
