use formality_core::id;
use formality_macros::term;
use formality_types::grammar::{Parameter, Ty};

use crate::grammar::FnId;

// This definition is based on [MiniRust](https://github.com/minirust/minirust/blob/master/spec/lang/syntax.md).

id!(BbId);
id!(LocalId);

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
    // StorageLive
    // StorageDead
}

/// Based on [MiniRust terminators](https://github.com/minirust/minirust/blob/9ae11cc202d040f08bc13ec5254d3d41d5f3cc25/spec/lang/syntax.md#statements-terminators).
#[term]
pub enum Terminator {
    #[grammar(goto $v0)]
    Goto(BbId),

    // Switch
    // Unreachable
    // Intrinsic

    // Example:
    //
    //    call foo(x, y)
    //    call foo.add<u32>(x, y)
    // FIXME: allow not having next block for goto, and add a comment for it.
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

#[term]
pub enum ArgumentExpression {
    #[grammar(Copy($v0))]
    ByValue(ValueExpression),
    #[grammar(Move($v0))]
    InPlace(PlaceExpression),
}

#[term]
pub enum ValueExpression {
    #[grammar(fn_id $v0)]
    Fn(FnId),
    // #[grammar($(v0) as $v1)]
    // Tuple(Vec<ValueExpression>, Ty),
    // Union
    // Variant
    // GetDiscriminant
    #[grammar(load($v0))]
    Load(PlaceExpression),
    // TODO: literals
    // AddrOf
    // UnOp
    // BinOp
}

#[term]
pub enum PlaceExpression {
    // FIXME(tiif): if we remove the local keyword, call bar () -> v1 goto bb1; won't work
    #[grammar(local($v0))]
    Local(LocalId),
    // Deref(Arc<ValueExpression>),
    // Field(Arc<PlaceExpression>, FieldId),
    // Index
    // Downcast
}
