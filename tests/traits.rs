#[test]
fn trait_with_valid_fn() {
    a_mir_formality::assert_ok!(
        [
            crate core {
                trait A {
                    fn a() -> ();
                }
            }
        ]
    );
}

#[test]
fn trait_with_valid_associated_type() {
    a_mir_formality::assert_ok!(
        [
            crate core {
                trait A {
                    type Assoc : [];
                }
            }
        ]
    );
}

#[test]
fn trait_with_ill_formed_where_clause() {
    a_mir_formality::assert_err!(
        [
            crate core {
                trait A<T> where T: B {}
                trait B {}
                trait WellFormed where for<T> u32: A<T> {}
            }
        ]
        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(A(u32, !ty_1)), via: A(u32, ?ty_2), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: B(!ty_0), via: A(u32, ?ty_1), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    );
}
