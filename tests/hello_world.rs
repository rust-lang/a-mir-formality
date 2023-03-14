#![allow(non_snake_case)]

use formality::test_program_ok;

const PROGRAM_BROKEN: &str = "[
    crate Foo {
        trait Foo<ty T> where [T: Bar<Self>] {}

        trait Bar<ty T> where [T: Baz<>] {}
        
        trait Baz<> where [] {}
    }
]";

const PROGRAM_OK: &str = "[
    crate Foo {
        trait Foo<ty T> where [T: Bar<Self>, Self: Baz<>] {}

        trait Bar<ty T> where [T: Baz<>] {}
        
        trait Baz<> where [] {}
    }
]";

#[test]
fn test_broken() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Foo)",
                source: "failed to prove {@ WellFormedTraitRef(Bar(!ty_2, !ty_1))} given [!ty_2 : Bar < !ty_1 >], got {}",
            },
        )
    "#]].assert_debug_eq(&test_program_ok(PROGRAM_BROKEN));
}

#[test]
fn test_ok() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(PROGRAM_OK));
}
