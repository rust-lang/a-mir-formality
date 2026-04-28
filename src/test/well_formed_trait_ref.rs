#[test]
fn dependent_where_clause() {
    crate::assert_ok!(

        [
            crate foo {
                trait Trait1 {}

                trait Trait2 {}

                struct S1<T> where T: Trait1 {
                    dummy: T,
                }

                struct S2<T> where T: Trait1, S1<T> : Trait2 {
                    dummy: T,
                }
            }
        ]
    )
}

#[test]
fn missing_dependent_where_clause() {
    crate::assert_err!(
        [
            crate foo {
                trait Trait1 {}

                trait Trait2 {}

                struct S1<T> where T: Trait1 {
                    dummy: T,
                }

                struct S2<T> where S1<T> : Trait2 {
                    dummy: T,
                }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(Trait2(S1<!ty_0>)), via: Trait2(S1<!ty_0>), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Trait1(!ty_0), via: Trait2(S1<!ty_0>), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Trait1(!ty_0), via: Place(?ty_1), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Trait1(!ty_0), via: Place(?ty_1), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Trait1(!ty_0), via: Place(?ty_1), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Trait1(!ty_0), via: PlaceRead(?ty_1, ?ty_2), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(S1<!ty_0>), via: Trait2(S1<!ty_0>), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
    )
}

#[test]
fn lifetime_param() {
    crate::assert_ok!(

        [
            crate foo {
                trait Trait1<'a> {}

                struct S1 {}

                struct S2<'a> where S1: Trait1<'a> {}
            }
        ]
    )
}

#[test]
fn static_lifetime_param() {
    crate::assert_ok!(

        [
            crate foo {
                trait Trait1<'a> {}

                struct S1 {}

                impl Trait1<'static> for S1 {}

                struct S2 where S1: Trait1<'static> {}
            }
        ]
    )
}

#[test]
fn const_param() {
    crate::assert_ok!(

        [
            crate foo {
                trait Trait1<const C> where type_of_const C is u32 {}

                struct S1 {}

                impl Trait1<u32(3)> for S1 {}

                struct S2 where S1: Trait1<u32(3)> {}
            }
        ]
    )
}

#[test]
#[should_panic(expected = "wrong number of parameters")]
fn type_with_wrong_number_of_parameters() {
    let _ = crate::test_program_ok(
        " [
            crate foo {
                trait Trait1 {}

                struct S1 {}

                struct S2<T> where S1<T> : Trait1 {
                    dummy: T,
                }
            }
        ] ",
    )
    .unwrap();
}

#[test]
#[should_panic(expected = "no ADT named `Nonex`")]
fn where_clause_with_nonexistent_type() {
    let _ = crate::test_program_ok(
        " [
            crate foo {
                trait Trait1 {}

                struct S1 where Nonex: Trait1 {}
            }
        ] ",
    )
    .unwrap();
}
