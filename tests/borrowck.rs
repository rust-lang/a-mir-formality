use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

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
            let x: Datum = Datum { value: 0_u32 };
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
            let x: Datum = Datum { value: 0_u32 };
            let y: Datum = x;
            x = Datum { value: 1_u32 };
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
                x = 1_u32;
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
                x = 1_u32;
            } else {
                x = 2_u32;
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
            x.first = 1_u32;
            return 0_u32;
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
            let x: Pair = Pair { first: Datum { value: 1_u32 }, second: Datum { value: 2_u32 } };
            let a: Datum = x.first;
            let b: Datum = x.second;
            return b;
        }
    }])
    .skip_execute()
    .ok()
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
            let x: Pair = Pair { first: Datum { value: 1_u32 }, second: Datum { value: 2_u32 } };
            let a: Datum = x.first;
            let b: Pair = x;
            return 0_u32;
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
            let x: Pair = Pair { first: Datum { value: 1_u32 }, second: Datum { value: 2_u32 } };
            let a: Datum = x.first;
            let b: Datum = x.first;
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
            let x: Pair = Pair { first: Datum { value: 1_u32 }, second: Datum { value: 2_u32 } };
            let a: Pair = x;
            let b: Datum = x.first;
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
            let x: Outer = Outer { foo: Inner { bar: 1_u32 } };
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
                        let x: Datum = Datum { value: 0_u32 };
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
                        let r: &'r0 mut Datum = &mut 'r1 x;
                        let y: Datum = *r;
                        return y;
                    }
                }
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Copy(<&?lt_0 mut Datum as Derefable>::Target), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Copy(<&?lt_0 mut Datum as Derefable>::Target), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = &?lt_2 ?ty_3, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = &?lt_2 ?ty_3, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_2 ?ty_3, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1, ?lt_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = (), via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = (), via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = bool, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = bool, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = i8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = i8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = isize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = isize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = u8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = u8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <&?lt_0 mut Datum as Derefable>::Target = usize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [Datum : ?lt_0, Datum : ?lt_0], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Datum = usize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: Datum, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: @ wf(?lt_1), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Copy(<&?lt_0 mut Datum as Derefable>::Target), assumptions: {@ wf(?lt_0), @ wf(?lt_1), Copy(<&?lt_0 mut Datum as Derefable>::Target)}, env: Env { variables: [?lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: true } }

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
                let x: Datum = Datum { value: 0_u32 };
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
            let x: Datum = Datum { value: 0_u32 };
            'l: loop {
                let y: Datum = x;
                break 'l;
            }
            return 0_u32;
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
                        let v1: i32 = 0_i32;
                        let v2: &'r0 mut i32 = &mut 'r1 v1;
                        // This should result in an error
                        v1 = 1_i32;
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
                        let v1: i32 = 0_i32;
                        let v2: &'r0 i32 = &'r1 v1;
                        v1 = 1_i32;
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
#[test]
fn min_problem_case_3() {
    // This doesn't pass in Rust, but it does in our formulation.
    FormalityTest::new(crates![crate Foo {
        struct Map { }

        fn min_problem_case_3<'a>(m: &'a mut Map) -> &'a mut Map {
            exists<'r0, 'r1> {
                let n: &'r0 mut Map = &mut 'r0 *m;
                if true {
                    return n;
                } else {
                    let o: &'r1 mut Map = &mut 'r1 *m;
                    return o;
                }
            }
        }
    }])
    .skip_execute()
    .ok()
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
                            let v1: i32 = 0_i32;
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
                    let v1: i32 = 22_i32;
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
                        let v2: &'r0 mut i32;
                        {
                            let v1: i32 = 0_i32;
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
                            let v1: i32 = 0_i32;
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

        fn min_problem_case_3<'a>(m: &'a mut Map) -> &'a mut Map {
            exists<'r0, 'r1> {
                let n: &'r0 mut Map = &mut 'r0 *m;
                if true {
                } else {
                }
                let o: &'r1 mut Map = &mut 'r1 *m;
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
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: @ wf(?lt_2), assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: @ wf(?lt_2), assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, @ wf(?lt_2)}, env: Env { variables: [!lt_0, !lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: @ wf(?lt_1), assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_0, !lt_2, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: u32 : !lt_0, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_0, !lt_2, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_0, !lt_2, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: @ wf(?lt_1), assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_0, !lt_2, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: u32 : !lt_0, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_0, !lt_2, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_0, !lt_2, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_2 : !lt_0, via: @ wf(?lt_1), assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_2, !lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_2 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_2, !lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_2, b: !lt_0, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_2, !lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_2 : !lt_0, via: @ wf(?lt_1), assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_2, !lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_2 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_2, !lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_2, b: !lt_0, assumptions: {u32 : !lt_0, @ wf(?lt_1)}, env: Env { variables: [!lt_2, !lt_0, ?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

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
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

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

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: !lt_0 : !lt_1, assumptions: {u32 : !lt_0, !lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: u32 : !lt_0, assumptions: {u32 : !lt_0, !lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {u32 : !lt_0, !lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: !lt_0 : !lt_1, assumptions: {u32 : !lt_0, !lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: u32 : !lt_0, assumptions: {u32 : !lt_0, !lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {u32 : !lt_0, !lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: !lt_0 : !lt_2, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: !lt_0 : !lt_2, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1, !lt_0 : !lt_2}, env: Env { variables: [!lt_0, !lt_2, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: !lt_1 : !lt_2, assumptions: {u32 : !lt_0, !lt_1 : !lt_2}, env: Env { variables: [!lt_1, !lt_2, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0, !lt_1 : !lt_2}, env: Env { variables: [!lt_1, !lt_2, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0, !lt_1 : !lt_2}, env: Env { variables: [!lt_1, !lt_2, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: !lt_1 : !lt_2, assumptions: {u32 : !lt_0, !lt_1 : !lt_2}, env: Env { variables: [!lt_1, !lt_2, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0, !lt_1 : !lt_2}, env: Env { variables: [!lt_1, !lt_2, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0, !lt_1 : !lt_2}, env: Env { variables: [!lt_1, !lt_2, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

// For `list: &mut Map`, borrow `&mut (*list).value` then assign to `list`.
#[test]
fn problem_case_4() {
    FormalityTest::new(crates![crate Foo {
        struct Map {
            value: u32,
         }

        fn min_problem_case_4<'a>(list: &'a mut Map, list2: &'a mut Map) -> u32 {
            exists<'r0> {
                let num: &'r0 mut u32 = &mut 'r0 (*list).value;
                list = &mut 'a *list2;
                num;
                return 0_u32;
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
                let a: u32 = 0_u32;
                let b: u32 = 0_u32;
                // In Rustc, the 1-tuple is needed for some reason
                // Niko does not 100% understand, else rustc is able to
                // see that this program is safe.
                let q: &'l_q mut u32 = &mut 'loan_0 a;
                let p: &'l_p mut u32 = &mut 'loan_1 a;
                if true {
                    p = &mut 'loan_1 a;
                    q = &mut 'loan_2 b;
                } else {
                    p = &mut 'loan_3 b;
                }
                *q = 1_u32;
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
                            let y: i32 = 0_i32;
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
                let x: i32 = 0_i32;
                let r: &'r0 i32;
                'a: loop {
                    r; // this *may* read from `y` in a previous iteration
                    let y: i32 = 0_i32;
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
                            let x: i32 = 0_i32;
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
                    let x: i32 = 0_i32;
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
                    let 'a: v: i32 = 0_i32;
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
                        let a: u32 = 22_u32;
                        let p: &'r0 u32 = &'r1 a;
                        'l: loop {
                            if true {
                                a = 23_u32;
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
/// fn foo() -> &'a Map {
///     let m: Map = Map {};
///     let n: &mut Map = &mut m;
///     if false {
///         return n;
///     } else {
///         let o: &mut Map = &mut m;
///         return o;
///     }
/// }
/// ```
#[test]
fn if_false_borrowck() {
    FormalityTest::new(crates![crate Foo {
        struct Map { }

        fn foo<'a>(m: &'a mut Map) -> &'a mut Map {
            exists<'r0, 'r1> {
                let n: &'r0 mut Map = &mut 'r0 *m;
                if false {
                    return n;
                } else {
                    let o: &'r1 mut Map = &mut 'r1 *m;
                    return o;
                }
            }
        }
    }])
    .skip_execute()
    .ok()
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
                        let a: u32 = 22_u32;
                        let b: u32 = 22_u32;
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
                let v: u32 = 7_u32;
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
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0, u32 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_1, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: u32 : !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {u32 : !lt_0}, env: Env { variables: [!lt_1, !lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

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
                let v: u32 = 1_u32;
                let p: &'r0 u32 = &'r1 v;
                foo(0_u32);
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
                fn foo<'a>(x: &'a mut u32) -> u32 {
                    exists {
                        *x = 1_u32;
                        return 1_u32;
                    }
                }

                fn bar() -> u32 {
                    exists<'r0, 'r1, 'r2> {
                        let v: u32 = 0_u32;
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
                let p: Point = Point { x: 0_u32, y: 0_u32 };
                let b1: &'r0 mut u32 = &mut 'r1 p.x;
                let b2: &'r2 mut u32 = &mut 'r3 p.y;
                *b1 = 1_u32;
                *b2 = 2_u32;
                return 0_u32;
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
                        let p: Point = Point { x: 0_u32, y: 0_u32 };
                        let b1: &'r0 mut u32 = &mut 'r1 p.x;
                        p.x = 1_u32;
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
                let v1: u32 = 22_u32;
                let v2: &'r0 mut u32 = &mut 'r1 v1;
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
                    value: &'a mut u32,
                }
                fn foo() -> u32 {
                    exists<'r0> {
                        let v1: u32 = 0_u32;
                        let w: Wrapper<'r0> = Wrapper::<'r0> { value: &mut 'r0 v1 };
                        v1 = 1_u32;
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
        fn reborrow<'a>(a: &'a mut u8) -> &'a mut u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                if true {
                    let b: &'r1 mut u8 = &mut 'r0 *a;
                    return b;
                } else { }

                let c: &'r3 mut u8 = &mut 'r2 *a;
                return c;
            }
        }
    }])
    .skip_execute()
    .ok();
}

// Divergent paths (aka return) should not propagate outlives, liveness
#[test]
fn outlive_before_return_does_not_affect_merged_paths() {
    FormalityTest::new(crates![crate Foo {
        fn reborrow<'a>(a: &'a mut u8) -> &'a mut u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                // This creates an outlives constraint
                let b: &'r1 mut u8 = &mut 'r0 *a;
                if true {
                    return b;
                } else {
                    // this means the loan remains live
                }

                // If the outlives constraint propagated here,
                // we would get an error.
                let c: &'r3 mut u8 = &mut 'r2 *a;
                return c;
            }
        }
    }])
    .skip_execute()
    .ok();
}

// Divergent paths (aka return) should not propagate outlives, liveness
#[test]
fn loan_before_return_does_not_affect_dead_code_after() {
    FormalityTest::new(crates![crate Foo {
        fn reborrow<'a>(a: &'a mut u8) -> &'a mut u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                let b: &'r1 mut u8 = &mut 'r0 *a;
                return b;
                let c: &'r3 mut u8 = &mut 'r2 *a;
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
        fn reborrow<'a>(a: &'a mut u8) -> &'a mut u8 {
            exists<'r0, 'r1, 'r2, 'r3> {
                if true {
                    let b: &'r1 mut u8 = &mut 'r0 *a;
                    return b;
                } else {
                    let c: &'r3 mut u8 = &mut 'r2 *a;
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
                        let x: u32 = 22_u32;
                        let p: &'r1 u32 = &'r0 x;
                        let q: &'r2 u32 = p;
                        x = 1_u32;
                        q;
                        return 0_u32;
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
                let x: u32 = 22_u32;
                let p: &'r1 u32 = &'r0 x;
                let q: &'r2 u32 = p;
                x = 1_u32;
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .ok()
}
