use formality_macros::Visit;

use crate::cast::{Upcast, UpcastFrom};

use super::Bool;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub enum ValTree {
    Leaf(Scalar),
    Branches(Vec<ValTree>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub struct Scalar {
    bits: u128,
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

impl UpcastFrom<Bool> for Scalar {
    fn upcast_from(term: Bool) -> Self {
        Scalar { bits: term as u128 }
    }
}
