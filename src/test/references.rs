/// See <https://github.com/rust-lang/a-mir-formality/issues/312> for more information.
#[test]
fn recursive_reference_validity() {
    crate::assert_err!(
        [
            crate core {
                trait Trait {}
                struct A<X> where X: Trait {}
                fn invalid<'a, X>(x: &'a A<X>) -> ()
                where
                    X: 'a,
                {}
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(&!lt_1 A<!ty_0>), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(&!lt_1 A<!ty_0>), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Trait(!ty_0), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(A<!ty_0>), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
    );
}

#[test]
fn reference_validity() {
    crate::assert_ok!(
        [
            crate core {
                trait Trait {}
                struct A<X> where X: Trait {}
                fn valid<'a, X>(x: &'a A<X>) -> ()
                where
                    X: 'a,
                    X: Trait,
                {}
            }
        ]
    );
}
