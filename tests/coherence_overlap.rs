#![allow(non_snake_case)] // we embed type names into the names for our test functions

use a_mir_formality::test_program_ok;
use formality_core::test_util::ResultTestExt;
use formality_macros::test;

#[test]
fn test_overlap_normalize_alias_to_LocalType() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<LocalType as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator {
                }

                trait Mirror {
                    type T : [];
                }
                
                impl<ty A> Mirror for A {
                    type T = A;
                }
                
                struct LocalType {}
                
                trait LocalTrait { }
                
                impl<ty T> LocalTrait for T where T: Iterator { }
                
                impl LocalTrait for <LocalType as Mirror>::T { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...on its own, this is OK. Figuring this out, though, requires proving
    // `<LocalType as Mirror>::T: Iterator` which requires normalizing
    // the alias to `LocalType`...

    test_program_ok(&gen_program("")).assert_ok(expect_test::expect!["()"]);

    // ...but it's an error if LocalType implements Iterator (figuring *this* out also
    // requires normalizing).

    test_program_ok(&gen_program("impl Iterator for LocalType {}")).assert_err(
        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }
            impl LocalTrait for <LocalType as Mirror>::T { }"#]],
    );
}

#[test]
fn test_overlap_alias_not_normalizable() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<T as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator {
                }

                trait Mirror {
                    type T : [];
                }
                
                impl<ty A> Mirror for A {
                    type T = A;
                }
                
                struct LocalType {}
                
                trait LocalTrait { }
                
                impl<ty T> LocalTrait for T where T: Iterator { }
                
                impl<ty T> LocalTrait for <T as Mirror>::T where T: Mirror { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...you get an error here, because a downstream crate could implement
    // trait for some local type, in which case it would overlap.

    test_program_ok(&gen_program("")).assert_err(expect_test::expect![[r#"
        impls may overlap:
        impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }
        impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { }"#]]);

    // ...and if there is at least one Iterator impl, we also flag an error.

    test_program_ok(&gen_program("impl Iterator for u32 {}")).assert_err(expect_test::expect![[
        r#"
        impls may overlap:
        impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }
        impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { }"#
    ]]);
}
