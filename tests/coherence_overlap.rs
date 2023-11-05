#![allow(non_snake_case)] // we embed type names into the names for our test functions

use a_mir_formality::test_program_ok;
use formality_macros::test;

#[test]
fn test_overlap_normalize_alias_to_LocalType() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<LocalType as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator<> {
                }

                trait Mirror<> {
                    type T<> : [];
                }
                
                impl<ty A> Mirror<> for A {
                    type T<> = A;
                }
                
                struct LocalType<> {}
                
                trait LocalTrait<> { }
                
                impl<ty T> LocalTrait<> for T where T: Iterator<> { }
                
                impl<> LocalTrait<> for <LocalType as Mirror>::T { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...on its own, this is OK. Figuring this out, though, requires proving
    // `<LocalType as Mirror>::T: Iterator` which requires normalizing
    // the alias to `LocalType`...

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(&gen_program("")));

    // ...but it's an error if LocalType implements Iterator (figuring *this* out also
    // requires normalizing).

    expect_test::expect![[r#"
        Err(
            "impls may overlap:\nimpl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }\nimpl LocalTrait for <LocalType as Mirror>::T { }",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(&gen_program(
        "impl<> Iterator<> for LocalType<> {}",
    )));
}

#[test]
fn test_overlap_alias_not_normalizable() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<T as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator<> {
                }

                trait Mirror<> {
                    type T<> : [];
                }
                
                impl<ty A> Mirror<> for A {
                    type T<> = A;
                }
                
                struct LocalType<> {}
                
                trait LocalTrait<> { }
                
                impl<ty T> LocalTrait<> for T where T: Iterator<> { }
                
                impl<ty T> LocalTrait<> for <T as Mirror>::T where T: Mirror<> { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...you might expect an error here, because we have an impl for all `T` and another
    // impl for all `T: Iterator`, but we don't flag it as one because
    // Iterator is a local trait and we can see that nobody has implemented it...
    //
    // FIXME: rustc DOES flag an error here. I think this is because the trait solver
    // refuses to solve `?X: Iterator`; we haven't implemented that rule and I haven't
    // decided how to think about it.

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(&gen_program("")));

    // ...as long as there is at least one Iterator impl, however, we do flag an error.

    expect_test::expect![[r#"
        Err(
            "impls may overlap:\nimpl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }\nimpl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { }",
        )
    "#]] // FIXME
    .assert_debug_eq(&test_program_ok(&gen_program(
        "impl<> Iterator<> for u32 {}",
    )));
}
