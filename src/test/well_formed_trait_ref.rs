#[test]
fn dependent_where_clause() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1 {}

                trait Trait2 {}

                struct S1<ty T> where T: Trait1 {
                    dummy: T,
                }

                struct S2<ty T> where T: Trait1, S1<T> : Trait2 {
                    dummy: T,
                }
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn missing_dependent_where_clause() {
    crate::assert_err!(
        [
            crate foo {
                trait Trait1 {}

                trait Trait2 {}

                struct S1<ty T> where T: Trait1 {
                    dummy: T,
                }

                struct S2<ty T> where S1<T> : Trait2 {
                    dummy: T,
                }
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            prove_where_clauses_well_formed([S1<!ty_1> : Trait2])

            Caused by:
                judgment `prove { goal: {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)}, adt S2 <ty> where {Trait2(S1<^ty0_0>)}], {Trait1, Trait2}, {S1, S2}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ WellFormedTraitRef(Trait2(S1<!ty_0>)), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait well formed" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_wf { goal: S1<!ty_0>, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "ADT" failed at step #3 (src/file.rs:LL:CC) because
                                judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness }, known_true: true, substitution: {} }, goal: {Trait1(!ty_0)}, assumptions: {Trait2(S1<!ty_0>)} }` failed at the following rule(s):
                                  the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment `prove { goal: {Trait1(!ty_0)}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)}, adt S2 <ty> where {Trait2(S1<^ty0_0>)}], {Trait1, Trait2}, {S1, S2}) }` failed at the following rule(s):
                                      failed at (src/file.rs:LL:CC) because
                                        judgment `prove_wc_list { goal: {Trait1(!ty_0)}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                          the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                            judgment `prove_wc { goal: Trait1(!ty_0), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                              the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                                expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn lifetime_param() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1<lt a> {}

                struct S1 {}

                struct S2<lt a> where S1: Trait1<a> {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn static_lifetime_param() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1<lt a> {}

                struct S1 {}

                impl Trait1<static> for S1 {}

                struct S2 where S1: Trait1<static> {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn const_param() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1<const C> where type_of_const C is u32 {}

                struct S1 {}

                impl Trait1<const 3_u32> for S1 {}

                struct S2 where S1: Trait1<const 3_u32> {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
#[should_panic(expected = "wrong number of parameters")]
fn type_with_wrong_number_of_parameters() {
    crate::test_program_ok(
        " [
            crate foo {
                trait Trait1 {}

                struct S1 {}

                struct S2<ty T> where S1<T> : Trait1 {
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
    crate::test_program_ok(
        " [
            crate foo {
                trait Trait1 {}

                struct S1 where Nonex: Trait1 {}
            }
        ] ",
    )
    .unwrap();
}
