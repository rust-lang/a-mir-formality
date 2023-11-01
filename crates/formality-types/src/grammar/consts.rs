mod valtree;

use super::{Parameter, Ty, Variable};
use formality_core::{term, DowncastTo, Upcast, UpcastFrom, Visit};
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

    pub fn valtree(vt: impl Upcast<ValTree>, ty: impl Upcast<Ty>) -> Self {
        Self::new(ConstData::Value(vt.upcast(), ty.upcast()))
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self.data() {
            ConstData::Value(_, _) => None,
            ConstData::Variable(var) => Some(*var),
        }
    }

    pub fn as_value(&self) -> Option<(ValTree, Ty)> {
        match self.data() {
            ConstData::Value(v, t) => Some((v.clone(), t.clone())),
            ConstData::Variable(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
pub enum ConstData {
    Value(ValTree, Ty),
    Variable(Variable),
}

impl UpcastFrom<Self> for ConstData {
    fn upcast_from(term: Self) -> Self {
        term
    }
}

impl DowncastTo<ConstData> for Const {
    fn downcast_to(&self) -> Option<ConstData> {
        Some(self.data().clone())
    }
}

impl DowncastTo<Const> for Parameter {
    fn downcast_to(&self) -> Option<Const> {
        match self {
            Parameter::Ty(_) | Parameter::Lt(_) => None,
            Parameter::Const(c) => Some(c.clone()),
        }
    }
}

impl DowncastTo<ConstData> for Parameter {
    fn downcast_to(&self) -> Option<ConstData> {
        let c: Const = self.downcast_to()?;
        c.downcast_to()
    }
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
        Self::new(ConstData::Value(term.upcast(), Ty::bool()))
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
