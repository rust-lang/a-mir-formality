#![cfg(test)]

use formality_macros::test;
use formality_types::parse::term;

use crate::grammar::Program;

#[test]
fn test_parse_rust_like_trait_impl_syntax() {
    let r: Program = term(
        "[
            crate core {
                impl<ty A, ty B> PartialEq<A> for B where [] {

                }
            }
        ]",
    );

    // Note: the for etc are correctly accounted.
    expect_test::expect![[r#"
        [crate core { impl <ty, ty> PartialEq < ^ty0_0 > for ^ty0_1 where [] { } }]
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_parse_rust_like_trait_syntax() {
    let r: Program = term(
        "[
            crate core {
                trait Foo<ty A> where [A : Bar<Self>] {

                }
            }
        ]",
    );

    // Note: two type parameters, and the 0th one is self:
    expect_test::expect![[r#"
        [crate core { trait Foo <ty, ty> where [^ty0_1 : Bar < ^ty0_0 >] { } }]
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_parse_rust_like_struct_syntax() {
    let r: Program = term(
        "[
            crate core {
                struct Foo<ty A> where [] {
                    a : A,
                }
            }
        ]",
    );

    // Note: two type parameters, and the 0th one is self:
    expect_test::expect![[r#"
        [crate core { struct Foo <ty> where [] { a : ^ty0_0 } }]
    "#]]
    .assert_debug_eq(&r);
}
