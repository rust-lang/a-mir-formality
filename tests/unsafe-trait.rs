use formality::test_program_ok;

#[test]
fn unsafe_trait_requires_unsafe_impl_err() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl( impl <> SendTrait < > for (rigid (adt SendStruct)) where [] { })",
                source: "the trait requires an `unsafe impl` declaration",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                unsafe trait SendTrait<> where [] {}
                struct SendStruct<> where [] {}
                impl<> SendTrait<> for SendStruct<> where [] {}
            }
        ]",
    ));
}

#[test]
fn unsafe_trait_requires_unsafe_impl() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                unsafe trait CoreTrait<> where [] {}
                struct CoreStruct<> where [] {}
                unsafe impl<> CoreTrait<> for CoreStruct<> where [] {}
            }
        ]",
    ));
}
