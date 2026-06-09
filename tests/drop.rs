use a_mir_formality::{crates, test_util::FormalityTest};

// ===================================================================
// Drop trait: valid impls
// ===================================================================

/// Basic Drop impl for a non-generic struct.
#[test]
fn drop_impl_simple_struct() {
    FormalityTest::new(crates![
        crate Foo {
            struct MyStruct {
                value: u32,
            }

            impl Drop for MyStruct {}
        }
    ])
    .skip_execute()
    .ok()
}

/// Drop impl for a generic struct with matching parameters.
#[test]
fn drop_impl_generic_struct() {
    FormalityTest::new(crates![
        crate Foo {
            trait Clone {}

            struct MyStruct<T> where T: Clone {
                value: T,
            }

            impl<T> Drop for MyStruct<T> where T: Clone {}
        }
    ])
    .skip_execute()
    .ok()
}

/// Drop impl for a generic struct with no where-clauses.
#[test]
fn drop_impl_generic_no_where_clauses() {
    FormalityTest::new(crates![
        crate Foo {
            struct Wrapper<T> {
                value: T,
            }

            impl<T> Drop for Wrapper<T> {}
        }
    ])
    .skip_execute()
    .ok()
}

/// Drop impl with fewer where-clauses than the struct.
/// FIXME: This should be invalid because the struct requires the bounds for
/// well-formedness, but without a `fn drop` method in the trait, the WF check
/// on `MyStruct<T>` is not triggered. This will be caught once we model the
/// drop method signature.
#[test]
fn drop_impl_subset_where_clauses() {
    FormalityTest::new(crates![
        crate Foo {
            trait Clone {}
            trait Debug {}

            struct MyStruct<T> where T: Clone, T: Debug {
                value: T,
            }

            // Impl has no where-clauses — but the struct requires them.
            impl<T> Drop for MyStruct<T> {}
        }
    ])
    .skip_execute()
    .ok()
}

// ===================================================================
// Drop trait: invalid impls
// ===================================================================

/// Drop impl with extra where-clause not on the struct.
#[test]
fn drop_impl_extra_where_clause() {
    FormalityTest::new(crates![
        crate Foo {
            trait Clone {}

            struct MyStruct<T> {
                value: T,
            }

            impl<T> Drop for MyStruct<T> where T: Clone {}
        }
    ])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: PlaceRead(?ty_1, ?ty_2), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: PlaceRead(?ty_1, ?ty_2), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

/// Drop impl for a concrete instantiation (not generic enough).
#[test]
fn drop_impl_concrete_type_param() {
    FormalityTest::new(crates![
        crate Foo {
            struct MyStruct<T> {
                value: T,
            }

            impl Drop for MyStruct<u32> {}
        }
    ])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: MyStruct<!ty_0> = MyStruct<u32>, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: MyStruct<!ty_0>, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: MyStruct<u32>, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: u32 = !ty_0, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Drop(MyStruct<!ty_0>), assumptions: {Drop(MyStruct<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Drop(MyStruct<!ty_0>), via: PlaceRead(?ty_1, ?ty_2), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

/// Drop impl for a non-ADT type (e.g., u32).
#[test]
fn drop_impl_for_non_adt() {
    FormalityTest::new(crates![
        crate Foo {
            impl Drop for u32 {}
        }
    ])
    .err(expect_test::expect![[r#"
        the rule "Drop impl is always applicable" at (impls.rs) failed because
          Drop impl self type must be a struct or enum, got `u32`"#]])
}
