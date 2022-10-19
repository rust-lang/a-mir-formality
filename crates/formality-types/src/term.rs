use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::{
    fold::Fold,
    grammar::{Lt, Ty},
    parse::Parse,
};

pub trait Term: Fold + Parse + Ord + Eq + Hash + Debug {}

impl<T: Term> Term for Vec<T> {}

impl<T: Term> Term for Option<T> {}

impl<T: Term> Term for Arc<T> {}

impl Term for Ty {}

impl Term for Lt {}

impl Term for usize {}

impl Term for u32 {}
