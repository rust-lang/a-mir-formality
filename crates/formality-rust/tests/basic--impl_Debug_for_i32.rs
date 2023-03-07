#![cfg(FIXME)]
#![allow(non_snake_case)]

const PROGRAM: &str = "[
    crate core {
        trait Debug<> where [] { }

        impl<> Debug<> for i32 where [] { }
    }
]";

#[test]
#[ignore]
fn test_i32() {
    expect_test::expect![[r#"
        Ok(
            yes,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_where_clause(
        PROGRAM,
        "i32: Debug<>",
    ));
}

#[test]
#[ignore]
fn test_u32() {
    expect_test::expect![[r#"
        Ok(
            no,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_where_clause(
        PROGRAM,
        "u32: Debug<>",
    ));
}
