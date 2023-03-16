use formality::test_where_clause;

#[test]
fn test_mirror_normalizes_u32_to_u32() {
    // Variant of test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
    // where there is a negative impl, so it is accepted.

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <ty> Mirror < > for ^ty0_0 where [] { type Assoc <> = ^ty1_0 where [] ; })",
                source: Error {
                    context: "check_associated_ty_value(type Assoc <> = !ty_1 where [] ;)",
                    source: "failed to prove {@ wf(!ty_1)} given {}, got {}",
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        "[
            crate core {
                trait Mirror<> where [] {
                    type Assoc<> : [] where [];
                }

                impl<ty T> Mirror<> for T where [] {
                    type Assoc<> = T where [];
                }
            }
        ]",
        "exists<ty T> {} => {<u32 as Mirror>::Assoc<> = T}",
    ));
}
