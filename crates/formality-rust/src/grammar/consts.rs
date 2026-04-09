use crate::grammar::expr::Block;
use crate::grammar::{ParameterKind, Parameters, RigidName, ScalarId};

use super::{Parameter, Variable};
use formality_core::{cast_impl, term};

#[term]
pub enum Const {
    // Sort of equivalent to `ValTreeKind::Branch`
    #[cast]
    RigidValue(RigidConstData),

    // Sort of equivalent to `ValTreeKind::Leaf`
    #[cast]
    Scalar(ScalarValue),

    /// A block expression that evaluates to a const value,
    /// e.g. `{ 22_usize }` in `Foo<{ 22_usize }>`.
    #[cast]
    Block(Block),

    #[variable(ParameterKind::Const)]
    Variable(Variable),
}

/// Temporary alias for migration.
pub type ConstData = Const;

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

cast_impl!((ScalarValue) <: (Const) <: (Parameter));
cast_impl!((RigidConstData) <: (Const) <: (Parameter));
