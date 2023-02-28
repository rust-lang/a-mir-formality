use std::sync::Arc;

use formality_macros::term;

use crate::{
    cast::{DowncastFrom, DowncastTo, Upcast, UpcastFrom},
    cast_impl,
    collections::{Set, SetExt},
    grammar::APR,
    set,
};

use super::{AtomicPredicate, AtomicRelation, Binder, BoundVar};

#[term]
pub struct Wcs {
    set: Set<Wc>,
}

impl Wcs {
    pub fn split_first(self) -> Option<(Wc, Wcs)> {
        let (wc, set) = self.set.split_first()?;
        Some((wc, set.upcast()))
    }

    pub fn union(&self, other: impl Upcast<Wcs>) -> Self {
        let other: Wcs = other.upcast();
        let set = self.set.iter().cloned().chain(other.set).collect();
        Wcs { set }
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

impl FromIterator<Wc> for Wcs {
    fn from_iter<T: IntoIterator<Item = Wc>>(iter: T) -> Self {
        Wcs {
            set: iter.into_iter().collect(),
        }
    }
}

#[term($data)]
pub struct Wc {
    data: Arc<WcData>,
}

impl Wc {
    pub fn data(&self) -> &WcData {
        &self.data
    }

    pub fn for_all(names: &[BoundVar], data: impl Upcast<Wc>) -> Self {
        WcData::ForAll(Binder::new(names, data.upcast())).upcast()
    }
}

#[term]
pub enum WcData {
    #[cast]
    Atomic(APR),

    #[grammar(for $v0)]
    ForAll(Binder<Wc>),

    #[grammar(implies($v0 => $v1))]
    Implies(Wc, Wc),
}

// ---

impl UpcastFrom<WcData> for Wc {
    fn upcast_from(v: WcData) -> Self {
        Wc { data: Arc::new(v) }
    }
}

impl UpcastFrom<Wc> for WcData {
    fn upcast_from(v: Wc) -> Self {
        v.data().clone()
    }
}

impl DowncastFrom<Wc> for WcData {
    fn downcast_from(t: &Wc) -> Option<Self> {
        Some(t.data().clone())
    }
}

// ---

cast_impl!((APR) <: (WcData) <: (Wc));
cast_impl!((AtomicRelation) <: (APR) <: (Wc));
cast_impl!((AtomicPredicate) <: (APR) <: (Wc));

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

cast_impl!((APR) <: (Wc) <: (Wcs));
cast_impl!((AtomicRelation) <: (Wc) <: (Wcs));
cast_impl!((AtomicPredicate) <: (Wc) <: (Wcs));
