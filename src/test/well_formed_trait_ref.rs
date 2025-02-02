#![allow(non_snake_case)]

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

        expect_test::expect![[r#"..."#]]
    )
}
