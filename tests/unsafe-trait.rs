use formality::test_program_ok;

#[test]
fn unsafe_trait_requires_unsafe_impl_err() {
    expect_test::expect![[r#"
        Err(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                unsafe trait SendTrait<> where [] {}
                struct SendStruct<> where [] {}
                impl<> SendTrait for SendStruct where [] {}
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
                unsafe trait SendTrait<> where [] {}
                struct SendStruct<> where [] {}
                unsafe impl<> SendTrait for SendStruct where [] {}
            }
        ]",
    ));
}
