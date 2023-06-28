use formality::test_program_ok;

#[test]
fn test_broken() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Foo)",
                source: "failed to prove {@ WellFormedTraitRef(Bar(!ty_2, !ty_1))} given {Bar(!ty_2, !ty_1)}, got {}",
            },
        )
    "#]].assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<ty T> where [T: Bar<Self>] {}
        
                trait Bar<ty T> where [T: Baz<>] {}
                
                trait Baz<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_ok() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<ty T> where [T: Bar<Self>, Self: Baz<>] {}
        
                trait Bar<ty T> where [T: Baz<>] {}
                
                trait Baz<> where [] {}

                impl<> Baz<> for u32 where [] {}

                impl<> Bar<u32> for u32 where [] {}
                impl<ty T> Bar<T> for () where [T: Baz<>] {}
            }
        ]",
    ));
}
