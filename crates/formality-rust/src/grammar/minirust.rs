use formality_core::{id, UpcastFrom};
use formality_macros::term;
use formality_types::grammar::{Parameter, ScalarId, Ty};

use crate::grammar::minirust::ConstTypePair::*;
use crate::grammar::FnId;

use std::sync::Arc;

// This definition is based on [MiniRust](https://github.com/minirust/minirust/blob/master/spec/lang/syntax.md).

id!(BbId);
id!(LocalId);
id!(FieldId);

// Example:
//
// fn foo() -> u32 = minirust(v1) -> v0 {
//   let v0: u32;
//   let v1: u32;
//
//   bb0:
//     v0 = v1;
//     return;
// }
//
// fn bar() -> u32 = minirust(v1) -> v0 {
//   let v0: u32;
//   let v1: u32;
//
//   bb0:
//     call foo (v1) -> v0 goto bb1;
//
//   bb1:
//     return;
// }

/// Based on [MiniRust statements](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
#[term(minirust($,args) -> $ret {
    $*locals
    $*blocks
})]
pub struct Body {
    pub args: Vec<LocalId>,
    pub ret: LocalId,
    pub locals: Vec<LocalDecl>,
    pub blocks: Vec<BasicBlock>,
}

/// Based on [MiniRust statements](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
#[term(let $id: $ty;)]
pub struct LocalDecl {
    pub id: LocalId,
    pub ty: Ty,
}

/// Based on [MiniRust statements](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
#[term($id: {statements {$*statements} $terminator;})]
pub struct BasicBlock {
    pub id: BbId,
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

/// Based on [MiniRust statements](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
#[term]
pub enum Statement {
    #[grammar($v0 = $v1;)]
    Assign(PlaceExpression, ValueExpression),

    // Represent let _ = place;
    #[grammar(place_mention($v0);)]
    PlaceMention(PlaceExpression),
    // SetDiscriminant
    // Validate
    // Deinit
    #[grammar(StorageLive($v0);)]
    StorageLive(LocalId),
    #[grammar(StorageDead($v0);)]
    StorageDead(LocalId),
}

/// Based on [MiniRust terminators](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
#[term]
pub enum Terminator {
    #[grammar(goto $v0)]
    Goto(BbId),

    // Suggestion: try list of a pair, or list of a struct?
    #[grammar(switch($switch_value) -> $[switch_targets] otherwise: $fallback)]
    Switch {
        switch_value: ValueExpression,
        switch_targets: Vec<SwitchTarget>,
        fallback: BbId,
    },
    // Unreachable
    // Intrinsic

    // Example:
    //
    //    call foo(x, y)
    //    call foo.add<u32>(x, y)
    #[grammar(call $callee $<?generic_arguments> $(arguments) -> $ret $:goto $next_block)]
    Call {
        /// What function or method to call.
        callee: ValueExpression,
        // cc: CallingConvention,
        generic_arguments: Vec<Parameter>,
        /// The function arguments to pass.
        arguments: Vec<ArgumentExpression>,
        /// The place to put the return value into.
        ret: PlaceExpression,
        /// The block to jump to when this call returns.
        /// In minirust, if this is None, then UB will be raised when the function returns.
        /// FIXME(tiif): should we have the same behaviour as minirust?
        next_block: Option<BbId>,
    },

    /// Return from the current function.
    #[grammar(return)]
    Return,
}

#[term(($value: $target))]
pub struct SwitchTarget {
    pub value: u64,
    pub target: BbId,
}

#[term]
pub enum ArgumentExpression {
    #[grammar(Copy($v0))]
    ByValue(ValueExpression),
    #[grammar(Move($v0))]
    InPlace(PlaceExpression),
}

#[term]
pub enum ValueExpression {
    #[grammar(constant($v0))]
    Constant(ConstTypePair),
    #[grammar(fn_id $v0)]
    Fn(FnId),
    #[grammar(struct {$,v0} as $v1)]
    Struct(Vec<ValueExpression>, Ty),
    // Union
    // Variant
    // GetDiscriminant
    #[grammar(load($v0))]
    Load(PlaceExpression),
    // AddrOf
    // UnOp
    // BinOp
}

#[term]
pub enum ConstTypePair {
    #[grammar($v0: u8)]
    U8(u8),
    #[grammar($v0: u16)]
    U16(u16),
    #[grammar($v0: u32)]
    U32(u32),
    #[grammar($v0: u64)]
    U64(u64),
    #[grammar($v0: usize)]
    Usize(usize),
    #[grammar($v0: i8)]
    I8(i8),
    #[grammar($v0: i16)]
    I16(i16),
    #[grammar($v0: i32)]
    I32(i32),
    #[grammar($v0: i64)]
    I64(i64),
    #[grammar($v0: isize)]
    Isize(isize),
    #[grammar($v0)]
    Bool(Bool),
}

#[term]
pub enum Bool {
    True,
    False,
}

impl ConstTypePair {
    pub fn get_ty(&self) -> Ty {
        match self {
            U8(_) => Ty::upcast_from(ScalarId::U8),
            U16(_) => Ty::upcast_from(ScalarId::U16),
            U32(_) => Ty::upcast_from(ScalarId::U32),
            U64(_) => Ty::upcast_from(ScalarId::U64),
            Usize(_) => Ty::upcast_from(ScalarId::Usize),
            I8(_) => Ty::upcast_from(ScalarId::I8),
            I16(_) => Ty::upcast_from(ScalarId::I16),
            I32(_) => Ty::upcast_from(ScalarId::I32),
            I64(_) => Ty::upcast_from(ScalarId::I64),
            Isize(_) => Ty::upcast_from(ScalarId::Isize),
            Bool(_) => Ty::upcast_from(ScalarId::Bool),
        }
    }
}

#[term]
pub enum PlaceExpression {
    #[grammar(local($v0))]
    Local(LocalId),
    // Deref(Arc<ValueExpression>),
    // Project to a field.
    #[grammar($v0)]
    Field(FieldProjection),
    // Index
    // Downcast
}

#[term($root.$index)]
pub struct FieldProjection {
    /// The place to base the projection on.
    pub root: Arc<PlaceExpression>,
    /// The field to project to.
    pub index: usize,
}
