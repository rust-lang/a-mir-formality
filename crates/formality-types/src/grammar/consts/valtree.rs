use formality_macros::Visit;

use crate::cast::{Upcast, UpcastFrom};

use super::{Bool, ConstData};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub enum ValTree {
    Leaf(Scalar),
    Branches(Vec<ValTree>),
}

impl std::fmt::Debug for ValTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Leaf(s) => s.fmt(f),
            Self::Branches(branches) => branches.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub struct Scalar {
    bits: u128,
}

impl std::fmt::Debug for Scalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.bits.fmt(f)
    }
}

impl Scalar {
    pub fn new(bits: u128) -> Self {
        Self { bits }
    }
}

impl UpcastFrom<Bool> for ValTree {
    fn upcast_from(term: Bool) -> Self {
        Scalar::upcast_from(term).upcast()
    }
}

impl UpcastFrom<Scalar> for ValTree {
    fn upcast_from(s: Scalar) -> Self {
        Self::Leaf(s)
    }
}

impl UpcastFrom<Scalar> for ConstData {
    fn upcast_from(s: Scalar) -> Self {
        ValTree::upcast_from(s).upcast()
    }
}

impl UpcastFrom<Bool> for Scalar {
    fn upcast_from(term: Bool) -> Self {
        Scalar { bits: term as u128 }
    }
}
