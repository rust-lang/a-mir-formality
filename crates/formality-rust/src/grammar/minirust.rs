use formality_core::{id, UpcastFrom};
use formality_macros::term;
use formality_types::grammar::{Binder, Lt, Parameter, RefKind, ScalarId, Ty};

use crate::grammar::minirust::ConstTypePair::*;
use crate::grammar::FnId;

use std::sync::Arc;

// This definition is based on [MiniRust](https://github.com/minirust/minirust/blob/master/spec/lang/syntax.md).
// Difference from minirust:
// * ValueExpression::Struct - In minirust, struct is represented as Tuple.

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
//

/// Based on [MiniRust statements](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
///
/// The `exists` is used to capture region inference variables.
/// In the compiler, every region that appears outside the signature
/// gets a fresh inference variable. In a-mir-formality, we let the
/// user tell us how many inference variables there are. Lucky us.
///
/// So a Rust function like
///
/// ```rust,ignore
/// fn pick<'a>(x: &'a (u32, u32)) -> &'a u32 { let tmp = &x.0; tmp }
/// ```
///
/// might wind up with MIR like this -- note the `r0` and `r1`
/// variables
///
/// ```rust,ignore
/// fn pick<'a>(v1) -> v0 = minirust(v1) -> v0 {
///   // Local variables that represent the return type
///   // and the function parameters are declared first.
///   //
///   // Their types do not have region inference variables
///   // in scope.
///   let v0: &'a u32;
///   let v1: &'a (u32, u32);
///
///   exists<'r0, 'r1> {
///     let v2: &'r0 u32;
///
///     bb0:
///       v2 = &'r1 v1.0
///       v0 = v2
///
///     bb1:
///       return;
///   }
/// }
/// ```
///
/// You can think of the role of the type checker as proving:
///
/// * For all `'a`
///     * There exists lifetimes `r0`, `r1`
///         * Such that the body is well-typed
#[term(minirust($,args) -> $ret {
    // First declare types etc parameters and return values
    $*params

    // then declare the body
    exists $binder
})]
pub struct Body {
    pub args: Vec<LocalId>,
    pub ret: LocalId,
    // params contain function parameters and return local
    // as their types does not contain region inference variable.
    pub params: Vec<LocalDecl>,
    pub binder: Binder<BodyBound>,
}

#[term({
    $*locals
    $*blocks
})]
pub struct BodyBound {
    // locals contain every locals other than function parameters and return local.
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
    // Nondeterminstically jump to one of the given blocks.
    //
    // In practice, the compiler "is very very likely" to always
    // select the first one. But the static analysis does not rely on that.
    //
    // NB: Diverges from MiniRust
    #[grammar(goto $,v0)]
    Goto(Vec<BbId>),

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

    // NB: We diverge from MiniRust here. In MiniRust,
    // AddrOf is used to create pointers/references and other things,
    // depending on the [PtrType], but that does not include
    // lifetimes or other information we need.
    //
    // [PtrType]: https://github.com/minirust/minirust/blob/master/spec/mem/pointer.md
    #[grammar(&$?v0 $v1 $v2)]
    Ref(RefKind, Lt, PlaceExpression),
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

    #[grammar(*($v0))] // TODO: change syntax?
    Deref(Arc<PlaceExpression>), // XXX: we depart from MiniRust here and require a place

    // Project to a field.
    #[grammar($v0)]
    Field(FieldProjection),
    // Index
    // Downcast
}

impl PlaceExpression {
    /// True if `self` is a prefix of `other` (e.g., `a.b` is a prefix of `a.b` and `a.b.c` but not `a`)
    pub fn is_prefix_of(&self, mut other: &PlaceExpression) -> bool {
        loop {
            if self == other {
                break true;
            }

            if let Some(p) = other.prefix() {
                other = p;
            } else {
                break false;
            }
        }
    }

    /// Returns the next prefix of `self` (if any)
    pub fn prefix(&self) -> Option<&PlaceExpression> {
        match self {
            PlaceExpression::Local(_) => None,
            PlaceExpression::Deref(base) => Some(base),
            PlaceExpression::Field(FieldProjection { root, index: _ }) => Some(root),
        }
    }

    /// Returns all prefixes of `self` (if any)
    pub fn all_prefixes(&self) -> Vec<&PlaceExpression> {
        let mut v = vec![self];
        let mut p = self;
        while let Some(q) = p.prefix() {
            v.push(q);
            p = q;
        }
        v
    }
}

#[term($root.$index)]
pub struct FieldProjection {
    /// The place to base the projection on.
    pub root: Arc<PlaceExpression>,
    /// The field to project to.
    pub index: usize,
}

impl UpcastFrom<LocalId> for PlaceExpression {
    fn upcast_from(term: LocalId) -> Self {
        PlaceExpression::Local(term)
    }
}
