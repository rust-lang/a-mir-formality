#[test]
fn dependent_where_clause() {
    crate::assert_ok!(

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
            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn lifetime_param() {
    crate::assert_ok!(

        [
            crate foo {
                trait Trait1<lt a> {}

                struct S1 {}

                struct S2<lt a> where S1: Trait1<a> {}
            }
        ]
    )
}

#[test]
fn static_lifetime_param() {
    crate::assert_ok!(

        [
            crate foo {
                trait Trait1<lt a> {}

                struct S1 {}

                impl Trait1<static> for S1 {}

                struct S2 where S1: Trait1<static> {}
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

                impl Trait1<const 3_u32> for S1 {}

                struct S2 where S1: Trait1<const 3_u32> {}
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
