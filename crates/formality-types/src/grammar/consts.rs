mod valtree;

use crate::cast::{Upcast, UpcastFrom};

use super::{Parameter, Variable};
use formality_macros::{term, Visit};
use std::sync::Arc;
pub use valtree::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub struct Const {
    data: Arc<ConstData>,
}
impl Const {
    pub fn data(&self) -> &ConstData {
        &self.data
    }

    pub fn new(data: impl Upcast<ConstData>) -> Self {
        Self {
            data: Arc::new(data.upcast()),
        }
    }
    pub fn as_variable(&self) -> Option<Variable> {
        match self.data() {
            ConstData::Value(_) => None,
            ConstData::Variable(var) => Some(var.clone()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub enum ConstData {
    Value(ValTree),
    Variable(Variable),
}

#[term]
pub enum Bool {
    #[grammar(true)]
    True,
    #[grammar(false)]
    False,
}

impl UpcastFrom<Bool> for Const {
    fn upcast_from(term: Bool) -> Self {
        Self::new(ValTree::upcast_from(term))
    }
}

impl UpcastFrom<ValTree> for ConstData {
    fn upcast_from(v: ValTree) -> Self {
        Self::Value(v)
    }
}

impl UpcastFrom<Variable> for ConstData {
    fn upcast_from(v: Variable) -> Self {
        Self::Variable(v)
    }
}

impl UpcastFrom<Const> for Parameter {
    fn upcast_from(term: Const) -> Self {
        Self::Const(term)
    }
}
