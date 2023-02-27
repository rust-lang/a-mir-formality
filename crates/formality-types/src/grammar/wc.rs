use std::sync::Arc;

use formality_macros::term;

use crate::{
    cast::{DowncastFrom, Upcast, UpcastFrom},
    cast_impl,
    grammar::APR,
    set,
};

use super::{AtomicPredicate, AtomicRelation, Binder, BoundVar};

#[term($data)]
pub struct WcList {
    data: Arc<WcListData>,
}

#[term]
pub enum WcListData {
    #[cast]
    Wc(Wc),

    #[grammar($v0, $v1)]
    And(WcList, WcList),

    #[grammar(true)]
    True,
}

impl WcList {
    pub fn data(&self) -> &WcListData {
        &self.data
    }

    pub fn and(self, other: impl Upcast<WcList>) -> WcList {
        let other: WcList = other.upcast();
        if let WcListData::True = self.data() {
            other
        } else if let WcListData::True = other.data() {
            self
        } else {
            WcListData::And(self, other).upcast()
        }
    }

    pub fn t() -> WcList {
        WcListData::True.upcast()
    }

    /// Iterate over this list of where-clauses
    pub fn iter(&self) -> impl Iterator<Item = Wc> {
        let mut stack = vec![self];
        let mut result = set![];
        while let Some(l) = stack.pop() {
            match l.data() {
                WcListData::Wc(w) => {
                    result.insert(w.clone());
                }
                WcListData::And(l1, l2) => {
                    stack.push(l1);
                    stack.push(l2);
                }
                WcListData::True => (),
            }
        }
        result.into_iter()
    }
}

impl IntoIterator for WcList {
    type Item = Wc;

    type IntoIter = Box<dyn Iterator<Item = Wc>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter())
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

    pub fn and(self, other: Wc) -> WcList {
        WcListData::And(self.upcast(), other.upcast()).upcast()
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

impl UpcastFrom<WcListData> for WcList {
    fn upcast_from(v: WcListData) -> Self {
        WcList { data: Arc::new(v) }
    }
}

impl UpcastFrom<WcList> for WcListData {
    fn upcast_from(v: WcList) -> Self {
        v.data().clone()
    }
}

impl DowncastFrom<WcList> for WcListData {
    fn downcast_from(t: &WcList) -> Option<Self> {
        Some(t.data().clone())
    }
}

cast_impl!((Wc) <: (WcListData) <: (WcList));
cast_impl!((WcData) <: (Wc) <: (WcList));
cast_impl!((APR) <: (WcData) <: (WcList));
cast_impl!((APR) <: (WcData) <: (Wc));
cast_impl!((AtomicRelation) <: (APR) <: (Wc));
cast_impl!((AtomicPredicate) <: (APR) <: (Wc));
cast_impl!((AtomicRelation) <: (Wc) <: (WcList));
cast_impl!((AtomicPredicate) <: (Wc) <: (WcList));

impl FromIterator<Wc> for WcList {
    fn from_iter<T: IntoIterator<Item = Wc>>(iter: T) -> Self {
        iter.into_iter().fold(WcList::t(), |l, v| l.and(v))
    }
}
