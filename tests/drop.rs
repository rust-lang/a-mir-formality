use a_mir_formality::{assert_err, assert_ok};

// ===================================================================
// Drop trait: valid impls
// ===================================================================

/// Basic Drop impl for a non-generic struct.
#[test]
fn drop_impl_simple_struct() {
    assert_ok!([
        crate Foo {
            struct MyStruct {
                value: u32,
            }

            impl Drop for MyStruct {
                fn drop(self_: MyStruct) -> () { trusted }
            }
        }
    ]);
}

/// Drop impl for a generic struct with matching parameters.
#[test]
fn drop_impl_generic_struct() {
    assert_ok!([
        crate Foo {
            trait Clone {}

            struct MyStruct<T> where T: Clone {
                value: T,
            }

            impl<T> Drop for MyStruct<T> where T: Clone {
                fn drop(self_: MyStruct<T>) -> () { trusted }
            }
        }
    ]);
}

/// Drop impl for a generic struct with no where-clauses.
#[test]
fn drop_impl_generic_no_where_clauses() {
    assert_ok!([
        crate Foo {
            struct Wrapper<T> {
                value: T,
            }

            impl<T> Drop for Wrapper<T> {
                fn drop(self_: Wrapper<T>) -> () { trusted }
            }
        }
    ]);
}

/// Drop impl with fewer where-clauses than the struct.
/// This is actually invalid because the struct requires the bounds for well-formedness.
#[test]
fn drop_impl_subset_where_clauses() {
    assert_err!(
        [
            crate Foo {
                trait Clone {}
                trait Debug {}

                struct MyStruct<T> where T: Clone, T: Debug {
                    value: T,
                }

                // Impl has no where-clauses — but the struct requires them.
                impl<T> Drop for MyStruct<T> {
                    fn drop(self_: MyStruct<T>) -> () { trusted }
                }
            }
        ]

        // Fails because the struct's well-formedness requires T: Clone, T: Debug.
        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: PlaceRead(?ty_1, ?ty_2), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
    );
}

// ===================================================================
// Drop trait: invalid impls
// ===================================================================

/// Drop impl with extra where-clause not on the struct.
#[test]
fn drop_impl_extra_where_clause() {
    assert_err!(
        [
            crate Foo {
                trait Clone {}

                struct MyStruct<T> {
                    value: T,
                }

                impl<T> Drop for MyStruct<T> where T: Clone {
                    fn drop(self_: MyStruct<T>) -> () { trusted }
                }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: Place(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Clone(!ty_0), via: PlaceRead(?ty_1, ?ty_2), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
    );
}

/// Drop impl for a concrete instantiation (not generic enough).
#[test]
fn drop_impl_concrete_type_param() {
    assert_err!(
        [
            crate Foo {
                struct MyStruct<T> {
                    value: T,
                }

                impl Drop for MyStruct<u32> {
                    fn drop(self_: MyStruct<u32>) -> () { trusted }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "trait impl" at (mod.rs) failed because
              Drop impl for `MyStruct` has different generic parameters than the type definition"#]]
    );
}

/// Drop impl for a non-ADT type (e.g., u32).
#[test]
fn drop_impl_for_non_adt() {
    assert_err!(
        [
            crate Foo {
                impl Drop for u32 {
                    fn drop(self_: u32) -> () { trusted }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "trait impl" at (mod.rs) failed because
              Drop impl self type must be a struct or enum, got `u32`"#]]
    );
}
