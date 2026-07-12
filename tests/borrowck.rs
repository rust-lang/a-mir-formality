use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

/// Wrap `body` (a sequence of crate items) in a crate, optionally enabling a
/// feature gate. Lets a test run the same program under several borrow-check
/// modes without duplicating it; the gate is one of the `*_GATE` constants
/// below.
fn feature_gate_program(gate: &str, body: &str) -> String {
    format!("[crate Foo {{ {gate} {body} }}]")
}

/// The default borrow-check mode (no feature gate), analogous to rustc NLL.
const NLL_GATE: &str = "";
/// Analogous to rustc's `-Z polonius=next` (the polonius alpha analysis).
const POLONIUS_ALPHA_GATE: &str = "#![feature(polonius_alpha)]";
/// Analogous to rustc's `-Z polonius=legacy` (the datalog implementation).
const POLONIUS_UNLOCKED_GATE: &str = "#![feature(polonius_unlocked)]";

// ===================================================================
// Initialization and move tracking tests
//
// These tests exercise the initialization/move analysis. Currently
// formality does not track initialization or moves, so tests that
// expect errors are #[ignore]d. Tests that expect success pass today
// (vacuously, since there's no init checking to reject them).
// ===================================================================

/// Use of an uninitialized variable should be an error.
///
/// ```rust,ignore
/// fn foo() -> u32 {
///     let x: u32;
///     x  // ERROR: x isn't initialized
/// }
/// ```
#[test]
fn use_of_uninitialized_variable() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            let x: u32;
            return x;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]])
}

/// Use of a moved variable should be an error.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// fn foo() -> Datum {
///     let x: Datum = Datum { value: 1 };
///     let _y: Datum = x;  // x moved here
///     let _z: Datum = x;  // ERROR: use of moved value
///     return _z;
/// }
/// ```
#[test]
fn use_of_moved_variable() {
    FormalityTest::new(crates![crate Foo {
        struct Datum {
            value: u32,
        }

        fn foo() -> Datum {
            let x: Datum = Datum { value: 0 _ u32 };
            let y: Datum = x;
            let z: Datum = x;
            return z;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]])
}

/// Re-initialization after move should be OK.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// fn foo() -> Datum {
///     let x: Datum = Datum { value: 1 };
///     let _y: Datum = x;       // x moved
///     x = Datum { value: 2 };  // re-init
///     let _z: Datum = x;       // OK
///     return _z;
/// }
/// ```
#[test]
fn reinit_after_move() {
    FormalityTest::new(crates![crate Foo {
        struct Datum {
            value: u32,
        }

        fn foo() -> Datum {
            let x: Datum = Datum { value: 0 _ u32 };
            let y: Datum = x;
            x = Datum { value: 1 _ u32 };
            let z: Datum = x;
            return z;
        }
    }])
    .skip_execute()
    .ok()
}

/// Conditional initialization in only one branch should be an error.
///
/// ```rust,ignore
/// fn foo() -> u32 {
///     let x: u32;
///     if true {
///         x = 1;
///     } else { }
///     return x;  // ERROR: possibly uninitialized
/// }
/// ```
#[test]
fn conditional_init_one_branch() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            let x: u32;
            if true {
                x = 1 _ u32;
            } else {
            }
            return x;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]])
}

/// Conditional initialization in both branches should be OK.
///
/// ```rust,ignore
/// fn foo() -> u32 {
///     let x: u32;
///     if true {
///         x = 1;
///     } else {
///         x = 2;
///     }
///     return x;  // OK
/// }
/// ```
#[test]
fn conditional_init_both_branches() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            let x: u32;
            if true {
                x = 1 _ u32;
            } else {
                x = 2 _ u32;
            }
            return x;
        }
    }])
    .skip_execute()
    .ok()
}

/// Assigning to a field of an uninitialized variable should be an error.
///
/// ```rust,ignore
/// fn foo() -> u32 {
///     let x: Pair;
///     x.first = 1;  // ERROR: x not initialized
///     return 0;
/// }
/// ```
#[test]
fn assign_field_of_uninitialized() {
    FormalityTest::new(crates![crate Foo {
        struct Pair {
            first: u32,
            second: u32,
        }

        fn foo() -> u32 {
            let x: Pair;
            x.first = 1 _ u32;
            return 0 _ u32;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]])
}

/// After a partial move, sibling fields should still be usable.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// struct Pair { first: Datum, second: Datum }
/// fn foo() -> Datum {
///     let x: Pair = Pair { first: Datum { value: 1 }, second: Datum { value: 2 } };
///     let _a: Datum = x.first;  // move x.first
///     let _b: Datum = x.second; // OK — sibling still initialized
///     return _b;
/// }
/// ```
#[test]
fn partial_move_use_sibling() {
    FormalityTest::new(crates![crate Foo {
                struct Datum {
                    value: u32,
                }

                struct Pair {
                    first: Datum,
                    second: Datum,
                }

                fn foo() -> Datum {
                    let x: Pair = Pair { first: Datum { value: 1 _ u32 }, second: Datum { value: 2 _ u32 } };
                    let a: Datum = x.first;
                    let b: Datum = x.second;
                    return b;
                }
            }]).skip_execute().ok()
}

/// After a partial move, using the whole struct should be an error.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// struct Pair { first: Datum, second: Datum }
/// fn foo() -> u32 {
///     let x: Pair = Pair { first: Datum { value: 1 }, second: Datum { value: 2 } };
///     let _a: Datum = x.first;  // move x.first
///     let _b: Pair = x;         // ERROR: partially moved
///     return 0;
/// }
/// ```
#[test]
fn partial_move_use_whole() {
    FormalityTest::new(crates![crate Foo {
                struct Datum {
                    value: u32,
                }

                struct Pair {
                    first: Datum,
                    second: Datum,
                }

                fn foo() -> u32 {
                    let x: Pair = Pair { first: Datum { value: 1 _ u32 }, second: Datum { value: 2 _ u32 } };
                    let a: Datum = x.first;
                    let b: Pair = x;
                    return 0 _ u32;
                }
            }]).err(expect_test::expect![[r#"
                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`"#]])
}

/// Moving the same field twice should be an error.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// struct Pair { first: Datum, second: Datum }
/// fn foo() -> Datum {
///     let x: Pair = Pair { first: Datum { value: 1 }, second: Datum { value: 2 } };
///     let _a: Datum = x.first;
///     let _b: Datum = x.first;  // ERROR: already moved
///     return _b;
/// }
/// ```
#[test]
fn move_same_field_twice() {
    FormalityTest::new(crates![crate Foo {
                struct Datum {
                    value: u32,
                }

                struct Pair {
                    first: Datum,
                    second: Datum,
                }

                fn foo() -> Datum {
                    let x: Pair = Pair { first: Datum { value: 1 _ u32 }, second: Datum { value: 2 _ u32 } };
                    let a: Datum = x.first;
                    let b: Datum = x.first;
                    return b;
                }
            }]).err(expect_test::expect![[r#"
                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`"#]])
}

/// Moving the whole variable should make fields inaccessible.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// struct Pair { first: Datum, second: Datum }
/// fn foo() -> Datum {
///     let x: Pair = Pair { first: Datum { value: 1 }, second: Datum { value: 2 } };
///     let _a: Pair = x;        // move whole struct
///     let _b: Datum = x.first;  // ERROR: x is moved
///     return _b;
/// }
/// ```
#[test]
fn move_whole_then_access_field() {
    FormalityTest::new(crates![crate Foo {
                struct Datum {
                    value: u32,
                }

                struct Pair {
                    first: Datum,
                    second: Datum,
                }

                fn foo() -> Datum {
                    let x: Pair = Pair { first: Datum { value: 1 _ u32 }, second: Datum { value: 2 _ u32 } };
                    let a: Pair = x;
                    let b: Datum = x.first;
                    return b;
                }
            }]).err(expect_test::expect![[r#"
                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`"#]])
}

/// Moving a parent field should make child fields inaccessible.
///
/// ```rust,ignore
/// fn foo() -> u32 {
///     let x: Outer = ...;
///     let _a: Inner = x.foo;    // move x.foo
///     let _b: u32 = x.foo.bar;  // ERROR: x.foo is moved
///     return _b;
/// }
/// ```
#[test]
fn move_parent_then_access_child() {
    FormalityTest::new(crates![crate Foo {
        struct Inner {
            bar: u32,
        }

        struct Outer {
            foo: Inner,
        }

        fn foo() -> u32 {
            let x: Outer = Outer { foo: Inner { bar: 1 _ u32 } };
            let a: Inner = x.foo;
            let b: u32 = x.foo.bar;
            return b;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]])
}

/// Cannot move out of a shared reference.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// fn foo() -> Datum {
///     let x: Datum = Datum { value: 1 };
///     let r: &Datum = &x;
///     let _y: Datum = *r;  // ERROR: cannot move out of &
///     return _y;
/// }
/// ```
#[test]
fn move_out_of_shared_ref() {
    FormalityTest::new(crates![crate Foo {
                struct Datum {
                    value: u32,
                }

                fn foo() -> Datum {
                    exists<'r0, 'r1> {
                        let x: Datum = Datum { value: 0 _ u32 };
                        let r: &'r0 Datum = &'r1 x;
                        let y: Datum = *r;
                        return y;
                    }
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Copy(<&?lt_0 Datum as Derefable>::Target), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Copy(<&?lt_0 Datum as Derefable>::Target), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = &?lt_2 ?ty_3, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = (), via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = bool, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = i8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = isize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = u8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 Datum as Derefable>::Target = usize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                the rule "trait implied bound" at (prove_wc.rs) failed because
                  expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// Cannot move out of a mutable reference.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// fn foo() -> Datum {
///     let x: Datum = Datum { value: 1 };
///     let r: &mut Datum = &mut x;
///     let _y: Datum = *r;  // ERROR: cannot move out of &mut
///     return _y;
/// }
/// ```
#[test]
fn move_out_of_mut_ref() {
    FormalityTest::new(crates![crate Foo {
                struct Datum {
                    value: u32,
                }

                fn foo() -> Datum {
                    exists<'r0, 'r1> {
                        let x: Datum = Datum { value: 0 _ u32 };
                        let r: &mut 'r0 Datum = &mut 'r1 x;
                        let y: Datum = *r;
                        return y;
                    }
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Copy(<&mut ?lt_0 Datum as Derefable>::Target), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Copy(<&mut ?lt_0 Datum as Derefable>::Target), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = &?lt_2 ?ty_3, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = (), via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = bool, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = i8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = isize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = u8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&mut ?lt_0 Datum as Derefable>::Target = usize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&mut ?lt_0 Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&mut ?lt_0 Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                the rule "trait implied bound" at (prove_wc.rs) failed because
                  expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// Cannot move a place that has an active loan.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// fn foo() -> Datum {
///     let x: Datum = Datum { value: 1 };
///     let r: &Datum = &x;
///     let _y: Datum = x;  // ERROR: x is borrowed
///     return *r;
/// }
/// ```
#[test]
fn move_out_of_borrowed_place() {
    FormalityTest::new(crates![crate Foo {
        struct Datum {
            value: u32,
        }

        fn foo() -> Datum {
            exists<'r0, 'r1> {
                let x: Datum = Datum { value: 0 _ u32 };
                let r: &'r0 Datum = &'r1 x;
                let y: Datum = x;
                return *r;
            }
        }
    }])
    .err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = x : Datum
                &access.place = x : Datum

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1"#]])
}

/// A move in a loop should be an error on the second iteration.
///
/// ```rust,ignore
/// struct Datum { value: u32 }
/// fn foo() -> u32 {
///     let x: Datum = Datum { value: 0 };
///     loop {
///         let _y: Datum = x;  // ERROR: moved in previous iteration
///         break;
///     }
///     return 0;
/// }
/// ```
///
/// FIXME: the uninit-set approach still can't catch this because `break`
/// causes the state to diverge (clearing `current` to default), so the
/// fixed-point iteration passes an empty uninit set to the next round.
/// Would need to merge break states into the loop entry state.
#[test]
#[ignore = "uninit-set: break diverges state, losing uninit set for fixed-point (#296)"]
fn move_in_loop() {
    FormalityTest::new(crates![crate Foo {
        struct Datum {
            value: u32,
        }

        fn foo() -> u32 {
            let x: Datum = Datum { value: 0 _ u32 };
            'l: loop {
                let y: Datum = x;
                break 'l;
            }
            return 0 _ u32;
        }
    }])
    .err(expect_test::expect![[""]])
}

/// Uninitialized return place should be an error.
///
/// ```rust,ignore
/// fn foo() -> u32 {
///     let x: u32;
///     return x;  // ERROR: x not initialized
/// }
/// ```
#[test]
fn uninitialized_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            let x: u32;
            return x;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]])
}
/// Test the holding a shared reference to a local
/// integer variable prevents it from being incremented.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn mutate() -> i32 {
///     let mut i = 0;
///     let j = &mut i;
///     i = 1; // <-- ERROR
///     *j
/// }
/// ```
#[test]
fn mutable_ref_prevents_mutation() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v1: i32 = 0 _ i32;
                        let v2: &mut 'r0 i32 = &mut 'r1 v1;
                        // This should result in an error
                        v1 = 1 _ i32;
                        return *v2;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]])
}

/// Test the holding a shared reference to a local
/// integer variable prevents it from being incremented.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn mutate() -> i32 {
///     let mut i = 0;
///     let j = &i;
///     i = 1; // <-- ERROR
///     *j
/// }
/// ```
#[test]
fn shared_ref_prevents_mutation() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v1: i32 = 0 _ i32;
                        let v2: &'r0 i32 = &'r1 v1;
                        v1 = 1 _ i32;
                        return *v2;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]])
}

/// Test the holding a shared reference to a local
/// integer variable prevents it from being incremented.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn min_problem_case_3(m: &mut Map) -> &mut Map {
///     let n: &mut Map = &mut *m;
///     if some_condition() {
///         return n;
///     } else {
///         let o: &mut Map = &mut *m;
///         return o;
///     }
/// }
/// ```
const MIN_PROBLEM_CASE_3: &str = "
    struct Map { }

    fn min_problem_case_3<'a>(m: &mut 'a Map) -> &mut 'a Map {
        exists<'r0, 'r1> {
            let n: &mut 'r0 Map = &mut 'r0 *m;
            if true {
                return n;
            } else {
                let o: &mut 'r1 Map = &mut 'r1 *m;
                return o;
            }
        }
    }
";

#[test]
fn min_problem_case_3() {
    FormalityTest::new(feature_gate_program(NLL_GATE, MIN_PROBLEM_CASE_3))
        .skip_execute()
        .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(m : &mut !lt_1 Map) : <&mut !lt_1 Map as Derefable>::Target
            &access.place = *(m : &mut !lt_1 Map) : <&mut !lt_1 Map as Derefable>::Target

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {!lt_1, ?lt_2}
            &lifetime.upcast() = !lt_1

        the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
          condition evaluated to false: `outlived_by_loan.iter().all(|p| match p
          {
              Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.as_ref()
              {
                  Lt::Static => false, Lt::Variable(Variable::UniversalVar(_)) => false,
                  Lt::Variable(Variable::ExistentialVar(_)) => true,
                  Lt::Variable(Variable::BoundVar(_)) =>
                  panic!("cannot outlive a bound var"), Lt::Erased => true,
              }, Parameter::Const(_) => panic!("cannot outlive a constant"),
          })`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `m`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(m : &mut !lt_1 Map) : <&mut !lt_1 Map as Derefable>::Target
            place_loaned_ref = m : &mut !lt_1 Map"#]]);

    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        MIN_PROBLEM_CASE_3,
    ))
    .skip_execute()
    .ok();

    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        MIN_PROBLEM_CASE_3,
    ))
    .skip_execute()
    .ok();
}

/// Test that dropping a borrowed variable is an error.
/// This is the expr-grammar equivalent of the old `storage_dead_while_borrowed` test.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v2: &i32;
///     {
///         let v1: i32 = 0;
///         v2 = &v1;     // borrow v1
///     }                  // v1 drops here — ERROR, still borrowed
///     *v2
/// }
/// ```
#[test]
fn drop_while_borrowed() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v2: &'r0 i32;
                        {
                            let v1: i32 = 0 _ i32;
                            v2 = &'r1 v1;
                        }
                        return *v2;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]])
}

/// Test that dropping a variable is fine when the borrow is no longer live.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let result: i32;
///     {
///         let v1: i32 = 22;
///         let v2: &i32 = &v1;
///         result = *v2;          // use the borrow
///     }                           // v1 drops here — OK, borrow is dead
///     result
/// }
/// ```
#[test]
fn drop_after_borrow_dead() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> i32 {
            exists<'r0, 'r1> {
                let result: i32;
                {
                    let v1: i32 = 22 _ i32;
                    let v2: &'r0 i32 = &'r1 v1;
                    result = *v2;
                }
                return result;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Test that dropping a mutably borrowed variable is an error.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v2: &mut i32;
///     {
///         let v1: i32 = 0;
///         v2 = &mut v1;  // mut borrow v1
///     }                   // v1 drops here — ERROR, still borrowed
///     *v2
/// }
/// ```
#[test]
fn drop_while_mutably_borrowed() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v2: &mut 'r0 i32;
                        {
                            let v1: i32 = 0 _ i32;
                            v2 = &mut 'r1 v1;
                        }
                        return *v2;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]])
}

/// Test that break out of a block drops locals in the exited scopes.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v2: &i32;
///     'a: {
///         let v1: i32 = 0;
///         v2 = &v1;         // borrow v1
///         break 'a;          // break exits 'a, dropping v1 — ERROR, v1 still borrowed
///     }
///     *v2
/// }
/// ```
#[test]
fn drop_on_break_while_borrowed() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v2: &'r0 i32;
                        'a: {
                            let v1: i32 = 0 _ i32;
                            v2 = &'r1 v1;
                            break 'a;
                        }
                        return *v2;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]])
}

/// A variant of Problem Case #3 which actually passes NLL
/// it doesn't exercise the interesting path.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn min_problem_case_3(m: &mut Map) -> &mut Map {
///     let n: &mut Map = &mut *m;
///     if some_condition() {
///     }
///     let o: &mut Map = &mut *m;
///     return o;
/// }
/// ```
#[test]
fn too_min_problem_case_3() {
    FormalityTest::new(crates![crate Foo {
        struct Map { }

        fn min_problem_case_3<'a>(m: &mut 'a Map) -> &mut 'a Map {
            exists<'r0, 'r1> {
                let n: &mut 'r0 Map = &mut 'r0 *m;
                if true {
                } else {
                }
                let o: &mut 'r1 Map = &mut 'r1 *m;
                return o;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Upcasting from `'a` to `'b` errors because
/// there is no declared relationship.
#[formality_core::test]
fn undeclared_universal_region_relationship() {
    FormalityTest::new(crates![crate Foo {
                fn foo<'a, 'b>(v1: &'a u32) -> &'b u32 {
                    exists<'r0> {
                        let v2: &'r0 u32 = v1;
                        return v2;
                    }
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_2, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_2, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_2, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_2, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

/// Same as `undeclared_universal_region_relationship`, but the function
/// loops forever and never returns. This tests whether the universal
/// outlives check fires even when there is no return terminator.
#[formality_core::test]
fn undeclared_universal_region_relationship_no_return() {
    FormalityTest::new(crates![crate Foo {
                fn foo<'a, 'b>(v1: &'a u32, v2: &'b u32) -> () {
                    let output: &'b u32 = v2;
                    loop {
                        output = v1;
                    }
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

/// Upcasting from `'a` to `'b` is allowed because
/// there is a declared relationship.
#[formality_core::test]
fn declared_universal_region_relationship() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'a, 'b>(v1: &'a u32) -> &'b u32
        where
            'a: 'b,
        {
            exists<'r0> {
                let v2: &'r0 u32 = v1;
                return v2;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Upcasting from `'a` to `'c` should be allowed because of
/// the transitive relationship.
#[formality_core::test]
fn declared_transitive_universal_region_relationship() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'a, 'b, 'c>(v1: &'a u32) -> &'c u32
        where
            'a: 'b,
            'b: 'c,
        {
            return v1;
        }
    }])
    .skip_execute()
    .ok()
}

/// Upcasting from `'a` to `'c` errors because of a missing
/// declared relationship to complete the transitive chain.
#[formality_core::test]
fn undeclared_transitive_universal_region_relationship() {
    FormalityTest::new(crates![crate Foo {
                fn foo<'a, 'b, 'c>(v1: &'a u32) -> &'c u32
                where
                    'a: 'b,
                {
                    return v1;
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

// For `list: &mut Map`, borrow `&mut (*list).value` then assign to `list`.
#[test]
fn problem_case_4() {
    FormalityTest::new(crates![crate Foo {
        struct Map {
            value: u32,
         }

        fn min_problem_case_4<'a>(list: &mut 'a Map, list2: &mut 'a Map) -> u32 {
            exists<'r0> {
                let num: &mut 'r0 u32 = &mut 'r0 (*list).value;
                list = &mut 'a *list2;
                num;
                return 0 _ u32;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Test that StorageDead on a borrowed variable is an error.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v1: i32 = 0;
///     let v2: &i32 = &v1;
///     StorageDead(v1);  // ERROR: v1 is still borrowed
///     *v2
/// }
/// ```
#[test]
#[ignore = "needs block scoping for storage dead semantics"]
fn storage_dead_while_borrowed() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> i32 = minirust {
            exists<'r0> {
                let v1: i32;
                let v2: &'r0 i32;

                bb0: {
                    statements {
                        local(v1) = constant(0: i32);
                        local(v2) = &'r0 local(v1);
                        StorageDead(v1);
                        local(_return) = load(*(local(v2)));
                    }
                    return;
                }
            }
        };
    }])
    .err(expect_test::expect![[r#"
            MaybeFnBody expected

            Caused by:
                0: = minirust
                       {
                           exists<'r0>
                           {
                               let v1: i32; let v2: &'r0 i32; bb0:
                               {
                                   statements
                                   {
                                       local(v1) = constant(0: i32); local(v2) = &'r0 local(v1);
                                       StorageDead(v1); local(_return) = load(*(local(v2)));
                                   } return;
                               }
                           }
                       };
                   }]
                1: failed to parse [crate Foo
                   {
                       fn foo() -> i32 = minirust
                       {
                           exists<'r0>
                           {
                               let v1: i32; let v2: &'r0 i32; bb0:
                               {
                                   statements
                                   {
                                       local(v1) = constant(0: i32); local(v2) = &'r0 local(v1);
                                       StorageDead(v1); local(_return) = load(*(local(v2)));
                                   } return;
                               }
                           }
                       };
                   }]"#]])
}

/// In this test, the write to `*(q.0)` is in fact safe,
/// but the borrow checker can't see it.
///
/// It is safe because, whichever value is returned by
/// `nondet()`, `p` and `q` remain disjoint.
/// But if you union the loans and assume both paths
/// may have been taken, then you get an error.
///
/// (In rustc, we do not get an error when `q` is just a
/// `&mut` local variable, we have to introduce the tuple,
/// so presumably something smart is happening around liveness
/// that I does not fully understand. --nikomatsakis)
#[test]
fn cfg_union_approx_cause_false_error() {
    /*
        #![allow(warnings)] // whiny rustc

    fn foo() -> u32 {
        let mut a = 0;
        let mut b = 0;
        let mut p;
        let mut q = (&mut a,);
        if nondet() {
            p = &mut a;
            q.0 = &mut b;
        } else {
            p = &mut b;
        }
        *(q.0) += 1;
        *p;
        // is there something that is "ok" if a is borrowed XOR b is borrowed?
        // but not if a is borrowed OR b is borrowed?
    }

    fn nondet() -> bool {
        true
    }
     */
    FormalityTest::new(crates![crate Foo {
        fn foo () -> u32 {
            exists<'l_p, 'l_q, 'loan_0, 'loan_1, 'loan_2, 'loan_3> {
                let a: u32 = 0 _ u32;
                let b: u32 = 0 _ u32;
                // In Rustc, the 1-tuple is needed for some reason
                // Niko does not 100% understand, else rustc is able to
                // see that this program is safe.
                let q: &mut 'l_q u32 = &mut 'loan_0 a;
                let p: &mut 'l_p u32 = &mut 'loan_1 a;
                if true {
                    p = &mut 'loan_1 a;
                    q = &mut 'loan_2 b;
                } else {
                    p = &mut 'loan_3 b;
                }
                *q = 1 _ u32;
                return *p;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// `continue` drops locals declared inside the loop body.
/// Borrowing a local and then continuing should fail if the
/// borrow escapes to a variable outside the loop.
#[test]
fn continue_drops_borrowed_local_false_edge() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let r: &'r0 i32;
                        'a: loop {
                            let y: i32 = 0 _ i32;
                            r = &'r1 y;
                            continue 'a;
                        }
                        r; // only an error because of false edges, assumption that all loops terminate
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = y : i32
                &access.place = y : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `y`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = y : i32
                &access.place = y : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `y`"#]]);
}

/// `continue` drops locals declared inside the loop body.
/// Borrowing a local and then continuing should fail if the
/// borrow escapes to a variable outside the loop.
#[test]
fn continue_drops_borrowed_local_loop_carried() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> i32 {
            exists<'r0, 'r1> {
                let x: i32 = 0 _ i32;
                let r: &'r0 i32;
                'a: loop {
                    r; // this *may* read from `y` in a previous iteration
                    let y: i32 = 0 _ i32;
                    r = &'r1 y;
                    continue 'a;
                }
            }
        }
    }])
    .err(expect_test::expect![[r#"
                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`

                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`

                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`

                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`"#]])
}

/// `break` drops locals declared inside the loop body.
/// Borrowing a local and then breaking should fail if the
/// borrow is used after the loop.
///
/// ```rust,compile_fail
/// fn foo() -> i32 {
///     let r: &i32;
///     'a: loop {
///         let x: i32 = 0;
///         r = &x;      // borrow x
///         break 'a;    // drops x while r is still live
///     }
///     *r
/// }
/// ```
#[test]
fn break_drops_borrowed_local() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let r: &'r0 i32;
                        'a: loop {
                            let x: i32 = 0 _ i32;
                            r = &'r1 x;
                            break 'a;
                        }
                        return *r;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = x : i32
                &access.place = x : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `x`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = x : i32
                &access.place = x : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `x`"#]])
}

/// Locals declared inside a loop are properly scoped:
/// a borrow that doesn't escape is fine even with continue.
///
/// ```rust
/// fn foo() {
///     'a: loop {
///         let x: i32 = 0;
///         let r: &i32 = &x;
///         let _ = *r;     // use borrow
///         continue 'a;    // r is dead, x can be dropped
///     }
/// }
/// ```
#[test]
fn continue_drops_local_borrow_dead() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                'a: loop {
                    let x: i32 = 0 _ i32;
                    let r: &'r0 i32 = &'r1 x;
                    let _y: i32 = *r;
                    continue 'a;
                }
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Locals declared inside a loop are properly scoped:
/// a borrow that doesn't escape is fine even with continue.
///
/// ```rust
/// fn foo() {
///     'a: loop {
///         let x: i32 = 0;
///         let r: &i32 = &x;
///         let _ = *r;     // use borrow
///         continue 'a;    // r is dead, x can be dropped
///     }
/// }
/// ```
#[test]
fn integer_in_outer_scope() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> () {
            'a: {
                {
                    let 'a: v: i32 = 0 _ i32;
                }
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Writing to a borrowed variable inside a loop before `continue`
/// should be an error when the borrow is live after the loop.
///
/// The liveness of `p` after the loop (used by `return *p`) should
/// propagate backward through the continue edge to the loop entry,
/// making `p` live at the point of `a = 23`. Since `p` borrows `a`,
/// the write to `a` conflicts with the live loan.
///
/// ```rust,compile_fail
/// fn foo() -> u32 {
///     let a: u32 = 22;
///     let p: &u32 = &a;
///     loop {
///         if true {
///             a = 23;       // ERROR: p's loan on a is live
///             continue;
///         }
///         break;
///     }
///     *p
/// }
/// ```
#[test]
fn write_to_borrowed_before_continue() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> u32 {
                    exists<'r0, 'r1> {
                        let a: u32 = 22 _ u32;
                        let p: &'r0 u32 = &'r1 a;
                        'l: loop {
                            if true {
                                a = 23 _ u32;
                                continue 'l;
                            } else {
                                break 'l;
                            }
                        }
                        return *p;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = a : u32
                &access.place = a : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = a : u32
                &access.place = a : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`"#]])
}

/// Test that `false` works as a condition in `if` with borrow checking.
/// The borrow checker must analyse both branches regardless of the
/// statically-known condition value.
///
/// ```rust,ignore
/// fn foo<'a>(m: &'a mut Map) -> &'a Map {
///     let mut n: &mut Map = &mut *m;
///     if false {
///         return n;
///     } else {
///         let o: &mut Map = &mut m;
///         return o;
///     }
/// }
/// ```
const IF_FALSE_BORROWCK: &str = "
    struct Map { }

    fn foo<'a>(m: &mut 'a Map) -> &mut 'a Map {
        exists<'r0, 'r1> {
            let n: &mut 'r0 Map = &mut 'r0 *m;
            if false {
                return n;
            } else {
                let o: &mut 'r1 Map = &mut 'r1 *m;
                return o;
            }
        }
    }
";

#[test]
fn if_false_borrowck() {
    FormalityTest::new(feature_gate_program(NLL_GATE, IF_FALSE_BORROWCK))
        .skip_execute()
        .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(m : &mut !lt_1 Map) : <&mut !lt_1 Map as Derefable>::Target
            &access.place = *(m : &mut !lt_1 Map) : <&mut !lt_1 Map as Derefable>::Target

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {!lt_1, ?lt_2}
            &lifetime.upcast() = !lt_1

        the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
          condition evaluated to false: `outlived_by_loan.iter().all(|p| match p
          {
              Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.as_ref()
              {
                  Lt::Static => false, Lt::Variable(Variable::UniversalVar(_)) => false,
                  Lt::Variable(Variable::ExistentialVar(_)) => true,
                  Lt::Variable(Variable::BoundVar(_)) =>
                  panic!("cannot outlive a bound var"), Lt::Erased => true,
              }, Parameter::Const(_) => panic!("cannot outlive a constant"),
          })`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `m`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(m : &mut !lt_1 Map) : <&mut !lt_1 Map as Derefable>::Target
            place_loaned_ref = m : &mut !lt_1 Map"#]]);

    FormalityTest::new(feature_gate_program(POLONIUS_ALPHA_GATE, IF_FALSE_BORROWCK))
        .skip_execute()
        .ok();

    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        IF_FALSE_BORROWCK,
    ))
    .skip_execute()
    .ok();
}

/// Writing to a borrowed variable before a loop that might not execute
/// should be an error, because the borrow is live along the zero-iteration path.
///
/// ```rust,ignore
/// let a = 2;
/// let b = 2;
/// let p = &a;
/// a = 3;        // <-- error: p borrows a, and if the loop runs 0 times, p is still &a
/// loop {
///     p = &b;   // kills p's borrow of a, but only if the loop runs
/// }
/// *p
/// ```
#[test]
fn write_to_borrowed_before_zero_iteration_loop() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> u32 {
                    exists<'r0, 'r1, 'r2> {
                        let a: u32 = 22 _ u32;
                        let b: u32 = 22 _ u32;
                        let p: &'r0 u32 = &'r1 a;
                        a = 23 _ u32;
                        'l: loop {
                            p = &'r2 b;
                            break 'l;
                        }
                        return *p;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = a : u32
                &access.place = a : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`"#]])
}

/// Test call to a generic function using turbofish syntax.
///
/// ```rust
/// fn identity<T>(v1: T) -> T {
///     return v1
/// }
///
/// fn foo<'a>(a: &'a u32) -> &'a u32 {
///     return identity(a)
/// }
/// ```
#[test]
fn call_generic_fn_with_turbofish() {
    FormalityTest::new(crates![crate Foo {
        fn identity<T>(v1: T) -> T {
            return v1;
        }

        fn foo<'a>(a: &'a u32) -> &'a u32 {
            exists<'r0> {
                let r: &'r0 u32 = identity::<&'r0 u32>(a);
                return r;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// pass &T to generic foo.
#[test]
fn call_pass_ref() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'a>(x: &'a u32) -> u32 {
            exists {
                return *x;
            }
        }

        fn bar() -> u32 {
            exists<'r1> {
                let v: u32 = 7 _ u32;
                let r: u32 = foo::<'r1>(&'r1 v);
                return r;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// Test call to a generic function using turbofish syntax and upcasting.
///
/// ```rust
/// fn identity<T>(v1: T) -> T {
///     return v1
/// }
///
/// fn foo<'a, 'b>(a: &'a u32) -> &'b u32 where 'a: 'b {
///     return identity::<&'b u32>(a)
/// }
/// ```
#[test]
fn call_generic_fn_with_turbofish_upcast() {
    FormalityTest::new(crates![crate Foo {
        fn identity<T>(v1: T) -> T {
            return v1;
        }

        fn foo<'a, 'b>(a: &'a u32) -> &'b u32
        where 'a: 'b {
            let r: &'b u32 = identity::<&'b u32>(a);
            return r;
        }
    }])
    .skip_execute()
    .ok()
}

/// Test call to a generic function using turbofish syntax and wrong lifetime.
///
/// ```rust
/// fn identity<T>(v1: T) -> T {
///     return v1
/// }
///
/// fn foo<'a, 'b>(a: &'a u32) -> &'b u32 {
///     return identity::<&'b u32>(a)
/// }
/// ```
#[test]
fn call_generic_fn_with_turbofish_missing_relation_upcast() {
    FormalityTest::new(crates![crate Foo {
                fn identity<T>(v1: T) -> T {
                    return v1;
                }

                fn foo<'a, 'b>(a: &'a u32) -> &'b u32 {
                    let r: &'b u32 = identity::<&'b u32>(a);
                    return r;
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

/// Test call to a generic function using turbofish syntax with lifetime and type.
#[test]
fn call_generic_fn_with_turbofish_lifetime_type() {
    FormalityTest::new(crates![crate Foo {
        fn bar<'a, T>(v1: T) -> T where T : 'a{
            return v1;
        }

        fn foo<'b>(a: &'b u32) -> &'b u32 {
            let r: &'b u32 = bar::<'b, &'b u32>(a);
            return r;
        }
    }])
    .skip_execute()
    .ok()
}

/// Call foo while p, &v is live then use p.
#[test]
fn call_while_borrow_live() {
    FormalityTest::new(crates![crate Foo {
        fn foo(x: u32) -> u32 {
            return x;
        }

        fn bar() -> u32 {
            exists<'r0, 'r1> {
                let v: u32 = 1 _ u32;
                let p: &'r0 u32 = &'r1 v;
                foo(0 _ u32);
                return *p;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// shared &v passing &mut v into foo in the same scope is a borrow error.
#[test]
fn call_mut_under_shared_borrow() {
    FormalityTest::new(crates![crate Foo {
                fn foo<'a>(x: &mut 'a u32) -> u32 {
                    exists {
                        *x = 1 _ u32;
                        return 1 _ u32;
                    }
                }

                fn bar() -> u32 {
                    exists<'r0, 'r1, 'r2> {
                        let v: u32 = 0 _ u32;
                        let p: &'r0 u32 = &'r1 v;
                        let _: u32 = foo::<'r2>(&mut 'r2 v);
                        return *p;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v : u32
                &access.place = v : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]])
}

// mutably borrowing two distinct fields of a struct -> assert_ok!
#[test]
fn struct_disjoint_field_borrows() {
    FormalityTest::new(crates![crate Foo {
        struct Point { x: u32, y: u32 }
        fn foo() -> u32 {
            exists<'r0, 'r1, 'r2, 'r3> {
                let p: Point = Point { x: 0 _ u32, y: 0 _ u32 };
                let b1: &mut 'r0 u32 = &mut 'r1 p.x;
                let b2: &mut 'r2 u32 = &mut 'r3 p.y;
                *b1 = 1 _ u32;
                *b2 = 2 _ u32;
                return 0 _ u32;
            }
        }
    }])
    .skip_execute()
    .ok()
}

/// accessing a field while it is already mutably borrowed -> borrow error
#[test]
fn struct_conflicting_field_borrows() {
    FormalityTest::new(crates![crate Foo {
                struct Point { x: u32, y: u32 }
                fn foo() -> u32 {
                    exists<'r0, 'r1> {
                        let p: Point = Point { x: 0 _ u32, y: 0 _ u32 };
                        let b1: &mut 'r0 u32 = &mut 'r1 p.x;
                        p.x = 1 _ u32;
                        return *b1;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = p : Point . x : u32
                &access.place = p : Point . x : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p : Point . x`"#]])
}

// constructing a struct reading a local variable that is mutably borrowed -> borrow error
#[test]
fn struct_construction_with_borrowed_local() {
    FormalityTest::new(crates![crate Foo {
        struct Wrapper {
            value: u32,
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v1: u32 = 22 _ u32;
                let v2: &mut 'r0 u32 = &mut 'r1 v1;
                let w: Wrapper = Wrapper { value: v1 };
                return *v2;
            }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v1 : u32
            &access.place = v1 : u32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_1, ?lt_2}
            &lifetime.upcast() = ?lt_1

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v1 : u32
            &access.place = v1 : u32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_1, ?lt_2}
            &lifetime.upcast() = ?lt_1"#]])
}

/// placing a mutable reference inside a struct -> locks the underlying local variable
#[test]
fn struct_with_mutable_reference_locks_local() {
    FormalityTest::new(crates![crate Foo {
                struct Wrapper<'a> {
                    value: &mut 'a u32,
                }
                fn foo() -> u32 {
                    exists<'r0> {
                        let v1: u32 = 0 _ u32;
                        let w: Wrapper<'r0> = Wrapper::<'r0> { value: &mut 'r0 v1 };
                        v1 = 1 _ u32;
                        return *(w.value);
                    }
                }
            }]).err(expect_test::expect![[r#"
                the rule "borrow of disjoint places" at (nll.rs) failed because
                  condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                    &loan.place = v1 : u32
                    &access.place = v1 : u32

                the rule "loan_cannot_outlive" at (nll.rs) failed because
                  condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                    outlived_by_loan = {?lt_1}
                    &lifetime.upcast() = ?lt_1

                the rule "write-indirect" at (nll.rs) failed because
                  pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]])
}

// Divergent paths (aka return) should not propagate outlives, liveness
#[test]
fn loan_before_return_does_not_affect_merged_paths() {
    FormalityTest::new(crates![crate Foo {
        fn reborrow<'a>(a: &mut 'a u8) -> &mut 'a u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                if true {
                    let b: &mut 'r1 u8 = &mut 'r0 *a;
                    return b;
                } else { }

                let c: &mut 'r3 u8 = &mut 'r2 *a;
                return c;
            }
        }
    }])
    .skip_execute()
    .ok();
}

// Divergent paths (aka return) should not propagate outlives, liveness
const OUTLIVE_BEFORE_RETURN_DOES_NOT_AFFECT_MERGED_PATHS: &str = "
    fn reborrow<'a>(a: &mut 'a u8) -> &mut 'a u8 {
        exists<'r0, 'r1, 'r2, 'r3> {
            // This creates an outlives constraint
            let b: &mut 'r1 u8 = &mut 'r0 *a;
            if true {
                return b;
            } else {
                // this means the loan remains live
            }

            // If the outlives constraint propagated here,
            // we would get an error.
            let c: &mut 'r3 u8 = &mut 'r2 *a;
            return c;
        }
    }
";

#[test]
fn outlive_before_return_does_not_affect_merged_paths() {
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        OUTLIVE_BEFORE_RETURN_DOES_NOT_AFFECT_MERGED_PATHS,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(a : &mut !lt_1 u8) : <&mut !lt_1 u8 as Derefable>::Target
            &access.place = *(a : &mut !lt_1 u8) : <&mut !lt_1 u8 as Derefable>::Target

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {!lt_1, ?lt_2, ?lt_3}
            &lifetime.upcast() = !lt_1

        the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
          condition evaluated to false: `outlived_by_loan.iter().all(|p| match p
          {
              Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.as_ref()
              {
                  Lt::Static => false, Lt::Variable(Variable::UniversalVar(_)) => false,
                  Lt::Variable(Variable::ExistentialVar(_)) => true,
                  Lt::Variable(Variable::BoundVar(_)) =>
                  panic!("cannot outlive a bound var"), Lt::Erased => true,
              }, Parameter::Const(_) => panic!("cannot outlive a constant"),
          })`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(a : &mut !lt_1 u8) : <&mut !lt_1 u8 as Derefable>::Target
            place_loaned_ref = a : &mut !lt_1 u8"#]]);

    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        OUTLIVE_BEFORE_RETURN_DOES_NOT_AFFECT_MERGED_PATHS,
    ))
    .skip_execute()
    .ok();

    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        OUTLIVE_BEFORE_RETURN_DOES_NOT_AFFECT_MERGED_PATHS,
    ))
    .skip_execute()
    .ok();
}

// Divergent paths (aka return) should not propagate outlives, liveness
#[test]
fn loan_before_return_does_not_affect_dead_code_after() {
    FormalityTest::new(crates![crate Foo {
        fn reborrow<'a>(a: &mut 'a u8) -> &mut 'a u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                let b: &mut 'r1 u8 = &mut 'r0 *a;
                return b;
                let c: &mut 'r3 u8 = &mut 'r2 *a;
                return c;
            }
        }
    }])
    .skip_execute()
    .ok();
}

// Divergent paths (aka return) should not propagate outlives, liveness
#[test]
fn if_else_paths_independent() {
    FormalityTest::new(crates![crate Foo {
        fn reborrow<'a>(a: &mut 'a u8) -> &mut 'a u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                if true {
                    let b: &mut 'r1 u8 = &mut 'r0 *a;
                    return b;
                } else {
                    let c: &mut 'r3 u8 = &mut 'r2 *a;
                    return c;
                }
            }
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
/// Fail test for loan_cannot_outlive's "lifetime" rule.
/// This is equivalent to:
/// ```
/// fn foo() -> u32 {
///     let mut x: u32 = 22;
///     let p = &x;
///     let q = p;
///     x += 1;
///     println!("{q}");
///     return 0;
/// }
/// ```
fn loan_cannot_outlive_lifetime_fail() {
    FormalityTest::new(crates![crate Foo {
                fn foo() -> u32 {
                    exists<'r0, 'r1, 'r2> {
                        let x: u32 = 22 _ u32;
                        let p: &'r1 u32 = &'r0 x;
                        let q: &'r2 u32 = p;
                        x = 1 _ u32;
                        q;
                        return 0 _ u32;
                    }
                }
            }]).err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = x : u32
                &access.place = x : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2, ?lt_3}
                &lifetime.upcast() = ?lt_3

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `x`"#]])
}

/// Pass test for loan_cannot_outlive's "lifetime" rule.
/// This is equivalent to:
/// ```
/// fn foo() -> u32 {
///     let mut x: u32 = 22;
///     let p = &x;
///     let q = p;
///     x += 1;
///     return 0;
/// }
/// ```
#[formality_core::test]
fn loan_cannot_outlive_lifetime_pass() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            exists<'r0, 'r1, 'r2> {
                let x: u32 = 22 _ u32;
                let p: &'r1 u32 = &'r0 x;
                let q: &'r2 u32 = p;
                x = 1 _ u32;
                return 0 _ u32;
            }
        }
    }])
    .skip_execute()
    .ok()
}

#[test]
fn live_loans() {
    FormalityTest::new(crates![crate Foo {
        trait Outlives<'a, 'b> where 'a: 'b {
            type Assoc : [];
        }
        struct S { }
        impl<'a, 'b> Outlives<'a, 'b> for S where 'a: 'b {
            type Assoc = u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v0: u32 = 0 _ u32;
                let v1: u32 = 0 _ u32;
                let a: &mut 'r0 u32 = &mut 'r0 v0;
                let b: &mut 'r1 u32 = &mut 'r1 v1;
                let x: <S as Outlives<'r0, 'r1>>::Assoc = 0 _ u32;
                *a = 1 _ u32;
                *b;
                x;
                return 0 _ u32;
            }
        }

    }])
    .skip_execute()
    .ok()
}

#[test]
fn live_places() {
    FormalityTest::new(crates![crate Foo {
        trait Outlives<'a, 'b> where 'a: 'b {
            type Assoc : [];
        }
        struct S { }
        impl<'a, 'b> Outlives<'a, 'b> for S where 'a: 'b {
            type Assoc = u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v0: u32 = 0 _ u32;
                let v1: u32 = 0 _ u32;
                let a: &mut 'r0 u32 = &mut 'r0 v0;
                let b: &mut 'r1 u32 = &mut 'r1 *a;
                *a = 1 _ u32;
                *b;
                return 0 _ u32;
            }
        }

    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(a : &mut ?lt_1 u32) : <&mut ?lt_1 u32 as Derefable>::Target
            &access.place = *(a : &mut ?lt_1 u32) : <&mut ?lt_1 u32 as Derefable>::Target

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_2}
            &lifetime.upcast() = ?lt_2

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(a : &mut ?lt_1 u32) : <&mut ?lt_1 u32 as Derefable>::Target
            place_loaned_ref = a : &mut ?lt_1 u32"#]])
}

// ===================================================================
// Ports of rustc NLL/polonius UI tests
//
// Each test below models the borrow-check essence of a rustc test from
// `tests/ui/nll/`. The formality expression grammar has no
// Option/Box/match/methods/Vec, so constructs like `Option<Box<Node>>`
// traversal are modeled with helper functions (some `trusted`) that
// produce references with the same derived-from/lifetime structure.
//
// Each program is run in three modes, mirroring the rustc revisions:
//   * no feature gate                  ~ [nll]
//   * #![feature(polonius_alpha)]      ~ [polonius] (-Z polonius=next)
//   * #![feature(polonius_unlocked)]   ~ [legacy]   (-Z polonius=legacy)
//
// The rustc-expected outcome for each revision is noted on each mode.
// Where formality currently deviates from rustc, the test records
// formality's actual behavior and the deviation is called out in a
// comment.
// ===================================================================

/// Port of `tests/ui/nll/issue-46589.rs` (`Foo::trigger_bug`).
///
/// ```rust,ignore
/// fn trigger_bug(&mut self) {
///     let other = &mut (&mut *self);
///     *other = match (*other).get_self() {
///         Some(s) => s,
///         None => (*other).new_self()
///     };
///     let c = other;
/// }
/// ```
///
/// The match scrutinee mutably reborrows `**other`; the `Some` arm stores
/// that reborrow into `*other`, while the `None` arm reborrows `**other` a
/// second time. `other` is used again afterwards. NLL ties the scrutinee
/// loan to `*other`'s region on the `Some` path, and flow-insensitively
/// flags the second reborrow on the `None` path.
///
/// rustc: [nll] error (known-bug #46589), [polonius] error (known-bug),
/// [legacy] pass.
const ISSUE_46589_TRIGGER_BUG: &str = "
    struct Foo { }

    fn get_self<'x>(x: &mut 'x Foo) -> &mut 'x Foo {
        return &mut 'x *x;
    }

    fn new_self<'x>(x: &mut 'x Foo) -> &mut 'x Foo {
        return &mut 'x *x;
    }

    fn trigger_bug<'s>(s: &mut 's Foo) -> u32 {
        exists<'r0, 'r1, 'r2, 'r3> {
            let tmp: &mut 'r0 Foo = &mut 'r0 *s;
            let other: &mut 'r1 &mut 'r0 Foo = &mut 'r1 tmp;
            let m: &mut 'r2 Foo = get_self::<'r2>(&mut 'r2 *(*other));
            if true {
                *other = m;
            } else {
                let n: &mut 'r3 Foo = new_self::<'r3>(&mut 'r3 *(*other));
                *other = n;
            }
            other;
            return 0 _ u32;
        }
    }
";

/// Blocked: a `let` whose annotated type is a nested reference (`&mut 'r1
/// &mut 'r0 Foo`) currently fails in every mode while proving
/// `@ wf(&mut ?lt_1 &mut ?lt_0 Foo)`: the proof has two incomparable
/// pending-outlives solutions and `proven_set` reports "no relationship
/// between ..." (a nondeterminism error, not a borrowck verdict). The same
/// happens with a `&mut 'r1 Wrapper<'r0>` struct wrapper.
#[test]
#[ignore = "nested reference `let` types hit ambiguous pending_outlives in prove_wf"]
fn issue_46589_trigger_bug() {
    // [nll]: rustc errors (known-bug #46589).
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_46589_TRIGGER_BUG))
        .skip_execute()
        .err(expect_test::expect![[""]]);

    // [polonius]: rustc errors (known-bug #46589).
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_46589_TRIGGER_BUG,
    ))
    .skip_execute()
    .err(expect_test::expect![[""]]);

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_46589_TRIGGER_BUG,
    ))
    .skip_execute()
    .ok();
}

/// Port of `tests/ui/nll/polonius/iterating-updating-cursor-issue-63908.rs`
/// (`remove_last_node_recursive`).
///
/// ```rust,ignore
/// fn remove_last_node_recursive<T>(node_ref: &mut List<T>) {
///     let next_ref = &mut node_ref.as_mut().unwrap().next;
///     if next_ref.is_some() {
///         remove_last_node_recursive(next_ref);
///     } else {
///         *node_ref = None;
///     }
/// }
/// ```
///
/// `next_of` models `&mut node_ref.as_mut().unwrap().next`: a reborrow of
/// the rest of the list derived from `*node`. The recursive version is
/// accepted by all analyses because the loan is dead on the `else` path.
///
/// rustc: [nll] pass, [polonius] pass, [legacy] pass.
const ISSUE_63908_REMOVE_LAST_NODE_RECURSIVE: &str = "
    struct List { value: u32 }

    fn next_of<'x>(l: &mut 'x List) -> &mut 'x List {
        return &mut 'x *l;
    }

    fn remove_last_node_recursive<'a>(node: &mut 'a List) -> u32 {
        exists<'r0> {
            let next: &mut 'r0 List = next_of::<'r0>(&mut 'r0 *node);
            if true {
                remove_last_node_recursive::<'r0>(next);
            } else {
                *node = List { value: 0 _ u32 };
            }
            return 0 _ u32;
        }
    }
";

#[test]
fn issue_63908_remove_last_node_recursive() {
    // [nll]: rustc passes.
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        ISSUE_63908_REMOVE_LAST_NODE_RECURSIVE,
    ))
    .skip_execute()
    .ok();

    // [polonius]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_63908_REMOVE_LAST_NODE_RECURSIVE,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_63908_REMOVE_LAST_NODE_RECURSIVE,
    ))
    .skip_execute()
    .ok();
}

/// Port of `tests/ui/nll/polonius/iterating-updating-cursor-issue-63908.rs`
/// (`remove_last_node_iterative`) -- the linked-list cursor pattern of
/// #46859/#48001.
///
/// ```rust,ignore
/// fn remove_last_node_iterative<T>(mut node_ref: &mut List<T>) {
///     loop {
///         let next_ref = &mut node_ref.as_mut().unwrap().next;
///         if next_ref.is_some() {
///             node_ref = next_ref;
///         } else {
///             break;
///         }
///     }
///     *node_ref = None;
/// }
/// ```
///
/// The cursor is advanced through a reborrow of its own referent inside a
/// loop, and written through after the loop.
///
/// rustc: [nll] error (known-bug #63908), [polonius] error (known-bug),
/// [legacy] pass.
const ISSUE_63908_REMOVE_LAST_NODE_ITERATIVE: &str = "
    struct List { value: u32 }

    fn remove_last_node_iterative<'a>(node: &mut 'a List) -> u32 {
        exists<'r0, 'r1> {
            let cursor: &mut 'r0 List = &mut 'r0 *node;
            'l: loop {
                let next: &mut 'r1 List = &mut 'r1 *cursor;
                if true {
                    cursor = next;
                } else {
                    break 'l;
                }
            }
            *cursor = List { value: 0 _ u32 };
            return 0 _ u32;
        }
    }
";

#[test]
fn issue_63908_remove_last_node_iterative() {
    // [nll]: rustc errors here (known-bug #63908): the `cursor = next`
    // subtyping constraint is location-insensitive, so the final
    // iteration's loan (which reaches the exit via the kill-free `break`
    // path) must cover the post-loop write, where the write's own use of
    // the cursor keeps its region live -> E0506. Formality reproduces this
    // via the gated live-before-at-access rule in `access_permitted_by_loan`
    // ("loan is dead"), which counts the accessed place's prefixes as live.
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        ISSUE_63908_REMOVE_LAST_NODE_ITERATIVE,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(cursor : &mut ?lt_2 List) : <&mut ?lt_2 List as Derefable>::Target
            &access.place = *(cursor : &mut ?lt_2 List) : <&mut ?lt_2 List as Derefable>::Target

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_2, ?lt_3}
            &lifetime.upcast() = ?lt_2

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `cursor`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(cursor : &mut ?lt_2 List) : <&mut ?lt_2 List as Derefable>::Target
            place_loaned_ref = cursor : &mut ?lt_2 List"#]]);

    // [polonius]: rustc errors here (known-bug #63908), same as [nll].
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_63908_REMOVE_LAST_NODE_ITERATIVE,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(cursor : &mut ?lt_2 List) : <&mut ?lt_2 List as Derefable>::Target
            &access.place = *(cursor : &mut ?lt_2 List) : <&mut ?lt_2 List as Derefable>::Target

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_2, ?lt_3}
            &lifetime.upcast() = ?lt_2

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `cursor`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(cursor : &mut ?lt_2 List) : <&mut ?lt_2 List as Derefable>::Target
            place_loaned_ref = cursor : &mut ?lt_2 List"#]]);

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_63908_REMOVE_LAST_NODE_ITERATIVE,
    ))
    .skip_execute()
    .ok();
}

/// Port of `tests/ui/nll/polonius/iterating-updating-cursor-issue-57165.rs`
/// (`no_control_flow`).
///
/// ```rust,ignore
/// let mut b = Some(Box::new(X { next: None }));
/// let mut p = &mut b;
/// while let Some(now) = p {
///     p = &mut now.next;
/// }
/// ```
///
/// The while-let borrow (`now`) is modeled as an explicit reborrow of `*p`
/// at the loop head; the cursor is unconditionally advanced from `now`.
///
/// rustc: [nll] pass, [polonius] pass, [legacy] pass.
const ISSUE_57165_NO_CONTROL_FLOW: &str = "
    struct X { value: u32 }

    fn next_of<'x>(x: &mut 'x X) -> &mut 'x X {
        return &mut 'x *x;
    }

    fn no_control_flow() -> u32 {
        exists<'r0, 'r1, 'r2, 'r3> {
            let b: X = X { value: 0 _ u32 };
            let p: &mut 'r0 X = &mut 'r1 b;
            'l: loop {
                let now: &mut 'r2 X = &mut 'r2 *p;
                if true {
                    let next: &mut 'r3 X = next_of::<'r3>(&mut 'r3 *now);
                    p = next;
                } else {
                    break 'l;
                }
            }
            return 0 _ u32;
        }
    }
";

#[test]
fn issue_57165_no_control_flow() {
    // [nll]: rustc passes.
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_57165_NO_CONTROL_FLOW))
        .skip_execute()
        .ok();

    // [polonius]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_57165_NO_CONTROL_FLOW,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_57165_NO_CONTROL_FLOW,
    ))
    .skip_execute()
    .ok();
}

/// Port of `tests/ui/nll/polonius/iterating-updating-cursor-issue-57165.rs`
/// (`conditional`).
///
/// ```rust,ignore
/// let mut p = &mut b;
/// while let Some(now) = p {
///     if true {
///         p = &mut now.next;
///     }
/// }
/// ```
///
/// Same as `no_control_flow` but the cursor advance is conditional, so the
/// loop-head borrow can flow around the back edge un-killed, which defeats
/// NLL's (and polonius alpha's) reachability approximation.
///
/// rustc: [nll] error (known-bug #57165), [polonius] error (known-bug),
/// [legacy] pass.
const ISSUE_57165_CONDITIONAL: &str = "
    struct X { value: u32 }

    fn next_of<'x>(x: &mut 'x X) -> &mut 'x X {
        return &mut 'x *x;
    }

    fn conditional() -> u32 {
        exists<'r0, 'r1, 'r2, 'r3> {
            let b: X = X { value: 0 _ u32 };
            let p: &mut 'r0 X = &mut 'r1 b;
            'l: loop {
                let now: &mut 'r2 X = &mut 'r2 *p;
                if true {
                    if true {
                        let next: &mut 'r3 X = next_of::<'r3>(&mut 'r3 *now);
                        p = next;
                    } else {
                    }
                } else {
                    break 'l;
                }
            }
            return 0 _ u32;
        }
    }
";

#[test]
fn issue_57165_conditional() {
    // [nll]: rustc errors (known-bug #57165).
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_57165_CONDITIONAL))
        .skip_execute()
        .err(expect_test::expect![[r#"
            the rule "fixed-point" at (nll.rs) failed because
              condition evaluated to false: `state0 == state1`
                state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_1)})
                state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut), loan(?lt_4, *(now : &mut ?lt_3 X) : <&mut ?lt_3 X as Derefable>::Target, mut)}), {labeled_flow_state('l, point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}))}, {}, {pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)})

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
                &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

            the rule "local" at (nll.rs) failed because
              unknown local variable `next_of`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
                place_loaned_ref = p : &mut ?lt_1 X

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
                &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

            the rule "local" at (nll.rs) failed because
              unknown local variable `next_of`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
                place_loaned_ref = p : &mut ?lt_1 X"#]]);

    // [polonius]: rustc errors (known-bug #57165).
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_57165_CONDITIONAL,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_1)})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut), loan(?lt_4, *(now : &mut ?lt_3 X) : <&mut ?lt_3 X as Derefable>::Target, mut)}), {labeled_flow_state('l, point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}))}, {}, {pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X"#]]);

    // [legacy]: rustc passes here.
    //
    // Deviation: formality's polonius_unlocked mode currently rejects this
    // program, unlike rustc's datalog implementation.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_57165_CONDITIONAL,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_1)})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut), loan(?lt_4, *(now : &mut ?lt_3 X) : <&mut ?lt_3 X as Derefable>::Target, mut)}), {labeled_flow_state('l, point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}))}, {}, {pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X"#]]);
}

/// Port of `tests/ui/nll/polonius/iterating-updating-cursor-issue-57165.rs`
/// (`conditional_with_indirection`).
///
/// ```rust,ignore
/// let mut p = &mut b;
/// while let Some(now) = p {
///     if true {
///         p = &mut p.as_mut().unwrap().next;
///     }
/// }
/// ```
///
/// Like `conditional`, but the advance reborrows `*p` directly rather than
/// going through `now`.
///
/// rustc: [nll] error (known-bug #57165), [polonius] error (known-bug),
/// [legacy] pass.
const ISSUE_57165_CONDITIONAL_WITH_INDIRECTION: &str = "
    struct X { value: u32 }

    fn next_of<'x>(x: &mut 'x X) -> &mut 'x X {
        return &mut 'x *x;
    }

    fn conditional_with_indirection() -> u32 {
        exists<'r0, 'r1, 'r2, 'r3> {
            let b: X = X { value: 0 _ u32 };
            let p: &mut 'r0 X = &mut 'r1 b;
            'l: loop {
                let now: &mut 'r2 X = &mut 'r2 *p;
                if true {
                    if true {
                        let next: &mut 'r3 X = next_of::<'r3>(&mut 'r3 *p);
                        p = next;
                    } else {
                    }
                } else {
                    break 'l;
                }
            }
            return 0 _ u32;
        }
    }
";

#[test]
fn issue_57165_conditional_with_indirection() {
    // [nll]: rustc errors (known-bug #57165).
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        ISSUE_57165_CONDITIONAL_WITH_INDIRECTION,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_1)})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}), {labeled_flow_state('l, point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}))}, {}, {pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X"#]]);

    // [polonius]: rustc errors (known-bug #57165).
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_57165_CONDITIONAL_WITH_INDIRECTION,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_1)})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}), {labeled_flow_state('l, point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}))}, {}, {pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X"#]]);

    // [legacy]: rustc passes here.
    //
    // Deviation: formality's polonius_unlocked mode currently rejects this
    // program, unlike rustc's datalog implementation.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_57165_CONDITIONAL_WITH_INDIRECTION,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_1)})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(some(U(4)), None, {}, None, [(b, X), (p, &mut ?lt_1 X)], [b : X, p : &mut ?lt_1 X]), scope(some(U(4)), Some('l), {}, Some({next_of, * p}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}), {labeled_flow_state('l, point_flow_state({pending_outlives(?lt_2, ?lt_1)}, {loan(?lt_2, b : X, mut), loan(?lt_3, *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target, mut)}))}, {}, {pending_outlives(?lt_2, ?lt_1), pending_outlives(?lt_4, ?lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            &access.place = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target

        the rule "local" at (nll.rs) failed because
          unknown local variable `next_of`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `p`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(p : &mut ?lt_1 X) : <&mut ?lt_1 X as Derefable>::Target
            place_loaned_ref = p : &mut ?lt_1 X"#]]);
}

/// Port of `tests/ui/nll/polonius/iterating-updating-mutref.rs` (`to_refs`,
/// the #46859 OP).
///
/// ```rust,ignore
/// fn to_refs<T>(mut list: &mut List<T>) -> Vec<&mut T> {
///     let mut result = vec![];
///     loop {
///         result.push(&mut list.value);
///         if let Some(n) = list.next.as_mut() {
///             list = n;
///         } else {
///             return result;
///         }
///     }
/// }
/// ```
///
/// The `Vec` of returned references is modeled as a single `&mut 'a u32`
/// that is re-assigned each iteration and returned: each `value` loan must
/// outlive the universal region `'a`, like a pushed element must. `next` is
/// a separate field so the `value` and `next` loans are disjoint, and
/// `next_from_field` (trusted) models `list.next.as_mut().unwrap()`.
///
/// rustc: [nll] pass, [polonius] pass, [legacy] pass. (The file's
/// `known-bug #46859` annotation comes from `Decoder::next` alone; the
/// `.nll.stderr` contains no error for `to_refs`, because the `list = n`
/// kill sits on the only back edge.)
const ISSUE_46859_TO_REFS: &str = "
    struct List { value: u32, next: u32 }

    fn next_from_field<'x>(n: &mut 'x u32) -> &mut 'x List { trusted }

    fn to_refs<'a>(list: &mut 'a List) -> &mut 'a u32 {
        exists<'r0, 'r1> {
            let result: &mut 'a u32;
            let cursor: &mut 'a List = &mut 'a *list;
            'l: loop {
                result = &mut 'r0 (*cursor).value;
                if true {
                    let n: &mut 'r1 List = next_from_field::<'r1>(&mut 'r1 (*cursor).next);
                    cursor = n;
                } else {
                    return result;
                }
            }
        }
    }
";

#[test]
fn issue_46859_to_refs() {
    // [nll]: rustc passes here (only `Decoder::next` errors in this file).
    //
    // Deviation: formality rejects this program. The `exists` redo pass
    // re-checks the block against the globalized `all_outlives` and the
    // still-live pass-1 loans, so the `&mut 'a *list` cursor-init borrow
    // conflicts with its own pass-1 loan (note loan.place == access.place
    // below), and `loan_cannot_outlive_universal_regions` condemns any
    // loan whose lifetime reaches `'a` regardless of where that constraint
    // arose.
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_46859_TO_REFS))
        .skip_execute()
        .err(expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(list : &mut !lt_1 List) : <&mut !lt_1 List as Derefable>::Target
                &access.place = *(list : &mut !lt_1 List) : <&mut !lt_1 List as Derefable>::Target

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {!lt_1}
                &lifetime.upcast() = !lt_1

            the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
              condition evaluated to false: `outlived_by_loan.iter().all(|p| match p
              {
                  Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.as_ref()
                  {
                      Lt::Static => false, Lt::Variable(Variable::UniversalVar(_)) => false,
                      Lt::Variable(Variable::ExistentialVar(_)) => true,
                      Lt::Variable(Variable::BoundVar(_)) =>
                      panic!("cannot outlive a bound var"), Lt::Erased => true,
                  }, Parameter::Const(_) => panic!("cannot outlive a constant"),
              })`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `list`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(list : &mut !lt_1 List) : <&mut !lt_1 List as Derefable>::Target
                place_loaned_ref = list : &mut !lt_1 List"#]]);

    // [polonius]: rustc passes here.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_46859_TO_REFS,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_46859_TO_REFS,
    ))
    .skip_execute()
    .ok();
}

/// Port of `tests/ui/nll/polonius/iterating-updating-mutref.rs` (`to_refs2`).
///
/// Same as `to_refs` but the loop exits via `break` and returns after the
/// loop; the constraint-graph paths clearly terminate, so even NLL accepts
/// it.
///
/// rustc: [nll] pass, [polonius] pass, [legacy] pass.
const ISSUE_46859_TO_REFS2: &str = "
    struct List { value: u32, next: u32 }

    fn next_from_field<'x>(n: &mut 'x u32) -> &mut 'x List { trusted }

    fn to_refs2<'a>(list: &mut 'a List) -> &mut 'a u32 {
        exists<'r0, 'r1> {
            let result: &mut 'a u32;
            let cursor: &mut 'a List = &mut 'a *list;
            'l: loop {
                result = &mut 'r0 (*cursor).value;
                if true {
                    let n: &mut 'r1 List = next_from_field::<'r1>(&mut 'r1 (*cursor).next);
                    cursor = n;
                } else {
                    break 'l;
                }
            }
            return result;
        }
    }
";

#[test]
fn issue_46859_to_refs2() {
    // [nll]: rustc passes.
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_46859_TO_REFS2))
        .skip_execute()
        .ok();

    // [polonius]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_46859_TO_REFS2,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_46859_TO_REFS2,
    ))
    .skip_execute()
    .ok();
}

/// Port of `tests/ui/nll/polonius/iterating-updating-mutref.rs`
/// (`Decoder::next`).
///
/// ```rust,ignore
/// pub fn next<'a>(&'a mut self) -> &'a str {
///     loop {
///         let buf = self.buf_read.fill_buf();
///         if let Some(s) = decode(buf) {
///             return s;
///         }
///         // loop to get more input data; `buf` is dead here, but NLL keeps
///         // `self.buf_read` borrowed around the back edge.
///     }
/// }
/// ```
///
/// rustc: [nll] error (known-bug #46859), [polonius] pass, [legacy] pass.
const ISSUE_46859_DECODER_NEXT: &str = "
    struct Decoder { buf_read: u32 }

    fn fill_buf<'x>(b: &mut 'x u32) -> &'x u32 { trusted }

    fn decode<'y>(b: &'y u32) -> &'y u32 { trusted }

    fn next<'a>(d: &mut 'a Decoder) -> &'a u32 {
        exists<'r0, 'r1> {
            'l: loop {
                let buf: &'r0 u32 = fill_buf::<'r0>(&mut 'r0 (*d).buf_read);
                let s: &'r1 u32 = decode::<'r1>(buf);
                if true {
                    return s;
                } else {
                }
            }
        }
    }
";

#[test]
fn issue_46859_decoder_next() {
    // [nll]: rustc errors (known-bug #46859).
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_46859_DECODER_NEXT))
        .skip_execute()
        .err(expect_test::expect![[r#"
            the rule "fixed-point" at (nll.rs) failed because
              condition evaluated to false: `state0 == state1`
                state0 = flow_state([scope(some(U(1)), None, {}, None, [(d, &mut !lt_1 Decoder)], [d : &mut !lt_1 Decoder]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(3)), None, {}, None, [], []), scope(some(U(3)), Some('l), {}, Some({decode, fill_buf, (* d) . buf_read}), [], [])], point_flow_state({}, {}), {}, {}, {})
                state1 = flow_state([scope(some(U(1)), None, {}, None, [(d, &mut !lt_1 Decoder)], [d : &mut !lt_1 Decoder]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(3)), None, {}, None, [], []), scope(some(U(3)), Some('l), {}, Some({decode, fill_buf, (* d) . buf_read}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_3)}, {loan(?lt_2, *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_3), pending_outlives(?lt_3, !lt_1)})

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
                &access.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32

            the rule "local" at (nll.rs) failed because
              unknown local variable `decode`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `d`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
                place_loaned_ref = d : &mut !lt_1 Decoder

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
                &access.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32

            the rule "local" at (nll.rs) failed because
              unknown local variable `decode`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `d`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
                place_loaned_ref = d : &mut !lt_1 Decoder"#]]);

    // [polonius]: rustc passes here.
    //
    // Deviation: formality's polonius_alpha mode currently rejects this
    // program, unlike rustc's -Z polonius=next. Note the failure now
    // starts with the "fixed-point" rule: the loop's flow state does not
    // converge (each iteration's loan of `(*d).buf_read`, tied to the
    // universal `'a` via the return path, keeps accumulating).
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_46859_DECODER_NEXT,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(some(U(1)), None, {}, None, [(d, &mut !lt_1 Decoder)], [d : &mut !lt_1 Decoder]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(3)), None, {}, None, [], []), scope(some(U(3)), Some('l), {}, Some({decode, fill_buf, (* d) . buf_read}), [], [])], point_flow_state({}, {}), {}, {}, {})
            state1 = flow_state([scope(some(U(1)), None, {}, None, [(d, &mut !lt_1 Decoder)], [d : &mut !lt_1 Decoder]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(3)), None, {}, None, [], []), scope(some(U(3)), Some('l), {}, Some({decode, fill_buf, (* d) . buf_read}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_3)}, {loan(?lt_2, *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_3), pending_outlives(?lt_3, !lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            &access.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `decode`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `d`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            place_loaned_ref = d : &mut !lt_1 Decoder

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            &access.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `decode`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `d`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            place_loaned_ref = d : &mut !lt_1 Decoder"#]]);

    // [legacy]: rustc passes here.
    //
    // Deviation: formality's polonius_unlocked mode currently rejects this
    // program, unlike rustc's datalog implementation.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_46859_DECODER_NEXT,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(some(U(1)), None, {}, None, [(d, &mut !lt_1 Decoder)], [d : &mut !lt_1 Decoder]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(3)), None, {}, None, [], []), scope(some(U(3)), Some('l), {}, Some({decode, fill_buf, (* d) . buf_read}), [], [])], point_flow_state({}, {}), {}, {}, {})
            state1 = flow_state([scope(some(U(1)), None, {}, None, [(d, &mut !lt_1 Decoder)], [d : &mut !lt_1 Decoder]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(3)), None, {}, None, [], []), scope(some(U(3)), Some('l), {}, Some({decode, fill_buf, (* d) . buf_read}), [], [])], point_flow_state({pending_outlives(?lt_2, ?lt_3)}, {loan(?lt_2, *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32, mut)}), {}, {}, {pending_outlives(?lt_2, ?lt_3), pending_outlives(?lt_3, !lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            &access.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `decode`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `d`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            place_loaned_ref = d : &mut !lt_1 Decoder

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            &access.place = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `decode`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `d`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(d : &mut !lt_1 Decoder) : <&mut !lt_1 Decoder as Derefable>::Target . buf_read : u32
            place_loaned_ref = d : &mut !lt_1 Decoder"#]]);
}

/// Port of `tests/ui/nll/polonius/filtering-lending-iterator-issue-92985.rs`
/// (`<Filter as LendingIterator>::next`), which is similar to NLL problem
/// case #3 inside a loop.
///
/// ```rust,ignore
/// fn next(&mut self) -> Option<I::Item<'_>> {
///     while let Some(item) = self.iter.next() {
///         if (self.predicate)(&item) {
///             return Some(item);
///         }
///     }
///     return None;
/// }
/// ```
///
/// `iter_next` models the lending iterator's `next` (the item borrows from
/// the iterator field), `call_predicate` models calling the boxed predicate
/// (a disjoint field), and `no_item` models `return None`.
///
/// rustc: [nll] error (known-bug #92985), [polonius] pass, [legacy] pass.
const ISSUE_92985_FILTER_NEXT: &str = "
    struct Filter { iter: u32, predicate: u32 }

    fn iter_next<'x>(i: &mut 'x u32) -> &mut 'x u32 { trusted }

    fn call_predicate<'p, 'i>(p: &mut 'p u32, item: &'i u32) -> bool { trusted }

    fn no_item<'x>() -> &mut 'x u32 { trusted }

    fn next<'s>(f: &mut 's Filter) -> &mut 's u32 {
        exists<'r0, 'r1, 'r2> {
            'l: loop {
                let item: &mut 'r0 u32 = iter_next::<'r0>(&mut 'r0 (*f).iter);
                if true {
                    let keep: bool = call_predicate::<'r1, 'r2>(&mut 'r1 (*f).predicate, &'r2 *item);
                    if keep {
                        return item;
                    } else {
                    }
                } else {
                    break 'l;
                }
            }
            return no_item::<'s>();
        }
    }
";

#[test]
fn issue_92985_filtering_lending_iterator() {
    // [nll]: rustc errors (known-bug #92985).
    FormalityTest::new(feature_gate_program(NLL_GATE, ISSUE_92985_FILTER_NEXT))
        .skip_execute()
        .err(expect_test::expect![[r#"
            the rule "fixed-point" at (nll.rs) failed because
              condition evaluated to false: `state0 == state1`
                state0 = flow_state([scope(some(U(1)), None, {}, None, [(f, &mut !lt_1 Filter)], [f : &mut !lt_1 Filter]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(4)), None, {}, None, [], []), scope(some(U(4)), Some('l), {no_item}, Some({call_predicate, iter_next, no_item, (* f) . iter, (* f) . predicate}), [], [])], point_flow_state({}, {}), {}, {}, {})
                state1 = flow_state([scope(some(U(1)), None, {}, None, [(f, &mut !lt_1 Filter)], [f : &mut !lt_1 Filter]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(4)), None, {}, None, [], []), scope(some(U(4)), Some('l), {no_item}, Some({call_predicate, iter_next, no_item, (* f) . iter, (* f) . predicate}), [], [])], point_flow_state({}, {loan(?lt_2, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32, mut), loan(?lt_3, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . predicate : u32, mut), loan(?lt_4, *(item : &mut ?lt_2 u32) : <&mut ?lt_2 u32 as Derefable>::Target, shared)}), {labeled_flow_state('l, point_flow_state({}, {loan(?lt_2, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32, mut)}))}, {}, {pending_outlives(?lt_2, !lt_1)})

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
                &access.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32

            the rule "local" at (nll.rs) failed because
              unknown local variable `call_predicate`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `f`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
                place_loaned_ref = f : &mut !lt_1 Filter

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
                &access.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32

            the rule "local" at (nll.rs) failed because
              unknown local variable `call_predicate`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `f`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                place_accessed = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
                place_loaned_ref = f : &mut !lt_1 Filter"#]]);

    // [polonius]: rustc passes here.
    //
    // Deviation: formality's polonius_alpha mode currently rejects this
    // program, unlike rustc's -Z polonius=next. As with
    // `issue_46859_decoder_next`, the failure now starts with the
    // "fixed-point" rule: the loop's flow state does not converge.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_92985_FILTER_NEXT,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(some(U(1)), None, {}, None, [(f, &mut !lt_1 Filter)], [f : &mut !lt_1 Filter]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(4)), None, {}, None, [], []), scope(some(U(4)), Some('l), {no_item}, Some({call_predicate, iter_next, no_item, (* f) . iter, (* f) . predicate}), [], [])], point_flow_state({}, {}), {}, {}, {})
            state1 = flow_state([scope(some(U(1)), None, {}, None, [(f, &mut !lt_1 Filter)], [f : &mut !lt_1 Filter]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(4)), None, {}, None, [], []), scope(some(U(4)), Some('l), {no_item}, Some({call_predicate, iter_next, no_item, (* f) . iter, (* f) . predicate}), [], [])], point_flow_state({}, {loan(?lt_2, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32, mut), loan(?lt_3, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . predicate : u32, mut), loan(?lt_4, *(item : &mut ?lt_2 u32) : <&mut ?lt_2 u32 as Derefable>::Target, shared)}), {labeled_flow_state('l, point_flow_state({}, {loan(?lt_2, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32, mut)}))}, {}, {pending_outlives(?lt_2, !lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            &access.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `call_predicate`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `f`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            place_loaned_ref = f : &mut !lt_1 Filter

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            &access.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `call_predicate`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `f`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            place_loaned_ref = f : &mut !lt_1 Filter"#]]);

    // [legacy]: rustc passes here.
    //
    // Deviation: formality's polonius_unlocked mode currently rejects this
    // program, unlike rustc's datalog implementation.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_92985_FILTER_NEXT,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(some(U(1)), None, {}, None, [(f, &mut !lt_1 Filter)], [f : &mut !lt_1 Filter]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(4)), None, {}, None, [], []), scope(some(U(4)), Some('l), {no_item}, Some({call_predicate, iter_next, no_item, (* f) . iter, (* f) . predicate}), [], [])], point_flow_state({}, {}), {}, {}, {})
            state1 = flow_state([scope(some(U(1)), None, {}, None, [(f, &mut !lt_1 Filter)], [f : &mut !lt_1 Filter]), scope(some(U(1)), None, {}, None, [], []), scope(some(U(4)), None, {}, None, [], []), scope(some(U(4)), Some('l), {no_item}, Some({call_predicate, iter_next, no_item, (* f) . iter, (* f) . predicate}), [], [])], point_flow_state({}, {loan(?lt_2, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32, mut), loan(?lt_3, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . predicate : u32, mut), loan(?lt_4, *(item : &mut ?lt_2 u32) : <&mut ?lt_2 u32 as Derefable>::Target, shared)}), {labeled_flow_state('l, point_flow_state({}, {loan(?lt_2, *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32, mut)}))}, {}, {pending_outlives(?lt_2, !lt_1)})

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            &access.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `call_predicate`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `f`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            place_loaned_ref = f : &mut !lt_1 Filter

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            &access.place = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32

        the rule "local" at (nll.rs) failed because
          unknown local variable `call_predicate`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `*(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter`

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `f`

        the rule "write-indirect" at (nll.rs) failed because
          condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
            place_accessed = *(f : &mut !lt_1 Filter) : <&mut !lt_1 Filter as Derefable>::Target . iter : u32
            place_loaned_ref = f : &mut !lt_1 Filter"#]]);
}

/// Port of `tests/ui/nll/polonius/flow-sensitive-invariance.rs` (`use_it`).
///
/// ```rust,ignore
/// struct Invariant<'l>(Cell<&'l ()>);
///
/// fn use_it<'a, 'b>(choice: bool) -> Result<Invariant<'a>, Invariant<'b>> {
///     let returned_value = create_invariant();
///     if choice { Ok(returned_value) } else { Err(returned_value) }
/// }
/// ```
///
/// Each branch forces the invariant lifetime of `returned_value` to equal a
/// different universal region; flow-sensitively each path only requires one
/// equality, but a flow-insensitive analysis requires `'a == 'b`. The
/// `Result` is modeled by passing the value to a sink instantiated at `'a`
/// on one path and `'b` on the other. Formality does not yet implement
/// variance (FIXME #220) -- ADT lifetime parameters are not invariant -- so
/// the invariance is modeled explicitly: `sink`'s `'x: 'y, 'y: 'x` bounds
/// force the value's region to *equal* the chosen universal region at each
/// call site.
///
/// rustc: [nll] error, [polonius] error, [legacy] pass.
const FLOW_SENSITIVE_INVARIANCE_USE_IT: &str = "
    struct Invariant<'l> { value: &'l u32 }

    fn create_invariant<'l>() -> Invariant<'l> { trusted }

    fn sink<'x, 'y>(v: Invariant<'y>) -> u32 where 'x: 'y, 'y: 'x { trusted }

    fn use_it<'a, 'b>() -> u32 {
        exists<'r0> {
            let v: Invariant<'r0> = create_invariant::<'r0>();
            if true {
                return sink::<'a, 'r0>(v);
            } else {
                return sink::<'b, 'r0>(v);
            }
        }
    }
";

#[test]
fn flow_sensitive_invariance_use_it() {
    // [nll]: rustc errors.
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        FLOW_SENSITIVE_INVARIANCE_USE_IT,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_2, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_2, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);

    // [polonius]: rustc errors here.
    //
    // Deviation: formality's polonius_alpha mode currently accepts this
    // program. rustc's alpha analysis keeps NLL's location-insensitive
    // outlives constraints (only loan liveness is computed via localized
    // reachability), so the `'r0 == 'a` / `'r0 == 'b` equalities from the
    // two branches still combine and conflict there; formality's alpha,
    // without the global-outlives rerun, keeps the constraints per-path
    // and each branch is satisfiable on its own.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        FLOW_SENSITIVE_INVARIANCE_USE_IT,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        FLOW_SENSITIVE_INVARIANCE_USE_IT,
    ))
    .skip_execute()
    .ok();
}

/// Companion check for `flow_sensitive_invariance_use_it`: with the
/// explicitly-modeled invariance, requiring `'r0 == 'a` and `'r0 == 'b` on
/// a *single* path must be an error in every mode (this guards that the
/// `sink` encoding really enforces equality, since ADT parameters are not
/// otherwise invariant in formality).
const FLOW_SENSITIVE_INVARIANCE_USE_BOTH: &str = "
    struct Invariant<'l> { value: &'l u32 }

    fn create_invariant<'l>() -> Invariant<'l> { trusted }

    fn sink<'x, 'y>(v: Invariant<'y>) -> u32 where 'x: 'y, 'y: 'x { trusted }

    fn use_both<'a, 'b>() -> u32 {
        exists<'r0> {
            let v: Invariant<'r0> = create_invariant::<'r0>();
            sink::<'a, 'r0>(v);
            sink::<'b, 'r0>(v);
            return 0 _ u32;
        }
    }
";

#[test]
fn flow_sensitive_invariance_use_both() {
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        FLOW_SENSITIVE_INVARIANCE_USE_BOTH,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_2, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_2, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);

    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        FLOW_SENSITIVE_INVARIANCE_USE_BOTH,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_2, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_2, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);

    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        FLOW_SENSITIVE_INVARIANCE_USE_BOTH,
    ))
    .skip_execute()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_2, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_2, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, !lt_2, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

/// Port of `tests/ui/nll/polonius/flow-sensitive-invariance.rs`
/// (`use_it_but_its_the_same_region`).
///
/// Same as `use_it`, but with `'a: 'b, 'b: 'a` declared the two equalities
/// are mutually satisfiable, so every analysis accepts it.
///
/// rustc: [nll] pass, [polonius] pass, [legacy] pass.
const FLOW_SENSITIVE_INVARIANCE_SAME_REGION: &str = "
    struct Invariant<'l> { value: &'l u32 }

    fn create_invariant<'l>() -> Invariant<'l> { trusted }

    fn sink<'x, 'y>(v: Invariant<'y>) -> u32 where 'x: 'y, 'y: 'x { trusted }

    fn use_it_but_its_the_same_region<'a, 'b>() -> u32 where 'a: 'b, 'b: 'a {
        exists<'r0> {
            let v: Invariant<'r0> = create_invariant::<'r0>();
            if true {
                return sink::<'a, 'r0>(v);
            } else {
                return sink::<'b, 'r0>(v);
            }
        }
    }
";

#[test]
fn flow_sensitive_invariance_same_region() {
    // [nll]: rustc passes.
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        FLOW_SENSITIVE_INVARIANCE_SAME_REGION,
    ))
    .skip_execute()
    .ok();

    // [polonius]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        FLOW_SENSITIVE_INVARIANCE_SAME_REGION,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        FLOW_SENSITIVE_INVARIANCE_SAME_REGION,
    ))
    .skip_execute()
    .ok();
}

/// Port of
/// `tests/ui/nll/polonius/location-insensitive-constraints-issue-70044.rs`.
///
/// ```rust,ignore
/// let mut zero = &mut 0;
/// let mut one = 1;
/// {
///     let mut _r = &mut zero;
///     let mut y = &mut one;
///     _r = &mut y;
/// }
/// println!("{}", one);  // NLL: cannot borrow `one` as immutable
/// println!("{}", zero);
/// ```
///
/// Storing `&mut y` into `_r: &mut &mut u32` equates (by invariance) `y`'s
/// region with `zero`'s inner region, which is live after the block. NLL's
/// constraints are location-insensitive, so `one` stays borrowed at the
/// first `println!` even though `_r` no longer points at `zero` when the
/// `&mut y` loan is created.
///
/// rustc: [nll] error, [polonius] pass, [legacy] pass.
const ISSUE_70044_LOCATION_INSENSITIVE_CONSTRAINTS: &str = "
    fn foo() -> u32 {
        exists<'r0, 'r1, 'r2, 'r3, 'r4> {
            let zero_v: u32 = 0 _ u32;
            let zero: &mut 'r0 u32 = &mut 'r1 zero_v;
            let one: u32 = 1 _ u32;
            {
                let r: &mut 'r2 &mut 'r0 u32 = &mut 'r2 zero;
                let y: &mut 'r3 u32 = &mut 'r3 one;
                r = &mut 'r4 y;
            }
            println!(one);
            println!(*zero);
            return 0 _ u32;
        }
    }
";

/// Blocked: same nested-reference `let` type limitation as
/// `issue_46589_trigger_bug` (`&mut 'r2 &mut 'r0 u32` fails WF-proving with
/// an ambiguous pending_outlives "no relationship" error in every mode).
/// The double indirection cannot be simplified away: `_r` no longer
/// pointing at `zero` when `&mut y` is stored is exactly what makes the
/// datalog analysis accept this program.
#[test]
#[ignore = "nested reference `let` types hit ambiguous pending_outlives in prove_wf"]
fn issue_70044_location_insensitive_constraints() {
    // [nll]: rustc errors.
    FormalityTest::new(feature_gate_program(
        NLL_GATE,
        ISSUE_70044_LOCATION_INSENSITIVE_CONSTRAINTS,
    ))
    .skip_execute()
    .err(expect_test::expect![[""]]);

    // [polonius]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_ALPHA_GATE,
        ISSUE_70044_LOCATION_INSENSITIVE_CONSTRAINTS,
    ))
    .skip_execute()
    .ok();

    // [legacy]: rustc passes.
    FormalityTest::new(feature_gate_program(
        POLONIUS_UNLOCKED_GATE,
        ISSUE_70044_LOCATION_INSENSITIVE_CONSTRAINTS,
    ))
    .skip_execute()
    .ok();
}
