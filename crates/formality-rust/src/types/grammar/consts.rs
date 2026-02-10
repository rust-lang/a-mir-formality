use crate::types::grammar::{minirust, ParameterKind, Parameters, RigidName, ScalarId};

use super::{Parameter, Variable};
use formality_core::{cast_impl, term, DowncastTo, Upcast, UpcastFrom};
use std::sync::Arc;

#[term]
#[cast]
#[customize(constructors)] // FIXME(#219): figure out upcasts with arc or special-case
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
            ConstData::Variable(v) => Some(v.clone()),
            _ => None,
        }
    }
}

#[term]
pub enum ConstData {
    // Sort of equivalent to `ValTreeKind::Branch`
    #[cast]
    RigidValue(RigidConstData),

    // Sort of equivalent to `ValTreeKind::Leaf`
    #[cast]
    Scalar(ScalarValue),

    #[cast]
    RvToTsv(minirust::Body),

    #[variable(ParameterKind::Const)]
    Variable(Variable),
}

#[term]
pub enum ScalarValue {
    #[grammar(u8($v0))]
    U8(u8),
    #[grammar(u16($v0))]
    U16(u16),
    #[grammar(u32($v0))]
    U32(u32),
    #[grammar(u64($v0))]
    U64(u64),
    #[grammar(i8($v0))]
    I8(i8),
    #[grammar(i16($v0))]
    I16(i16),
    #[grammar(i32($v0))]
    I32(i32),
    #[grammar(i64($v0))]
    I64(i64),
    #[grammar($v0)]
    Bool(bool),
    #[grammar(usize($v0))]
    Usize(usize),
    #[grammar(isize($v0))]
    Isize(isize),
}

impl ScalarValue {
    pub fn ty(&self) -> ScalarId {
        match self {
            ScalarValue::U8(_) => ScalarId::U8,
            ScalarValue::U16(_) => ScalarId::U16,
            ScalarValue::U32(_) => ScalarId::U32,
            ScalarValue::U64(_) => ScalarId::U64,
            ScalarValue::I8(_) => ScalarId::I8,
            ScalarValue::I16(_) => ScalarId::I16,
            ScalarValue::I32(_) => ScalarId::I32,
            ScalarValue::I64(_) => ScalarId::I64,
            ScalarValue::Bool(_) => ScalarId::Bool,
            ScalarValue::Usize(_) => ScalarId::Usize,
            ScalarValue::Isize(_) => ScalarId::Isize,
        }
    }
}

#[term($name $<parameters> { $,values })]
pub struct RigidConstData {
    pub name: RigidName,
    pub parameters: Parameters,
    pub values: Vec<Const>,
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

impl UpcastFrom<Const> for Parameter {
    fn upcast_from(term: Const) -> Self {
        Self::Const(term)
    }
}

impl UpcastFrom<ConstData> for Const {
    fn upcast_from(term: ConstData) -> Self {
        Const {
            data: Arc::new(term),
        }
    }
}

cast_impl!((ConstData) <: (Const) <: (Parameter));
cast_impl!((ScalarValue) <: (ConstData) <: (Const));
cast_impl!((minirust::Body) <: (ConstData) <: (Const));
