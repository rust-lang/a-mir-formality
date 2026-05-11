#![allow(non_snake_case)]

use a_mir_formality::{assert_err, assert_ok};

#[test]
fn unsafe_trait() {
    assert_ok!(

        [
            crate baguette {
                unsafe trait Foo {}
                unsafe impl Foo for u32 {}
            }
        ]
    )
}

#[test]
fn safe_trait() {
    assert_ok!(

        [
            crate baguette {
                safe trait Foo {}
                safe impl Foo for u32 {}
            }
        ]
    )
}

#[test]
fn unsafe_trait_negative_impl() {
    assert_ok!(

        [
            crate baguette {
                unsafe trait Foo {}
                impl !Foo for u32 {}
            }
        ]
    )
}

#[test]
fn unsafe_trait_negative_impl_mismatch() {
    assert_err!(
        [
            crate baguette {
                unsafe trait Foo {}
                unsafe impl !Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "neg trait impl" at (mod.rs) failed because
              check_neg_trait_impl(unsafe impl ! Foo for u32 {})

              Caused by:
                  negative impls cannot be unsafe"#]]
    )
}

#[test]
fn safe_trait_negative_impl_mismatch() {
    assert_err!(
        [
            crate baguette {
                trait Foo {}
                unsafe impl !Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "neg trait impl" at (mod.rs) failed because
              check_neg_trait_impl(unsafe impl ! Foo for u32 {})

              Caused by:
                  negative impls cannot be unsafe"#]]
    )
}

#[test]
fn unsafe_trait_mismatch() {
    assert_err!(
        [
            crate baguette {
                unsafe trait Foo {}
                impl Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "safety matches" at (impls.rs) failed because
              condition evaluated to false: `trait_decl.safety == trait_impl.safety`"#]]
    )
}

#[test]
fn safe_trait_mismatch() {
    assert_err!(
        [
            crate baguette {
                trait Foo {}
                unsafe impl Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "safety matches" at (impls.rs) failed because
              condition evaluated to false: `trait_decl.safety == trait_impl.safety`"#]]
    )
}
