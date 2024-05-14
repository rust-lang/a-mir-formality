#![allow(non_snake_case)]

#[test]
fn unsafe_trait() {
    crate::assert_ok!(
        //@check-pass
        [
            crate baguette {
                unsafe trait Foo {}
                unsafe impl Foo for u32 {}
            }
        ]
        
        expect_test::expect!["()"]
    )
}

#[test]
fn safe_trait() {
    crate::assert_ok!(
        //@check-pass
        [
            crate baguette {
                safe trait Foo {}
                safe impl Foo for u32 {}
            }
        ]
        
        expect_test::expect!["()"]
    )
}

#[test]
fn unsafe_trait_negative_impl() {
    crate::assert_ok!(
        //@check-pass
        [
            crate baguette {
                unsafe trait Foo {}
                impl !Foo for u32 {}
            }
        ]
        
        expect_test::expect!["()"]
    )
}

#[test]
fn unsafe_trait_negative_impl_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                unsafe trait Foo {}
                unsafe impl !Foo for u32 {}
            }
        ]
        
        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_neg_trait_impl(unsafe impl ! Foo for u32 {})

            Caused by:
                negative impls cannot be unsafe"#]]
    )
}

#[test]
fn safe_trait_negative_impl_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                trait Foo {}
                unsafe impl !Foo for u32 {}
            }
        ]
        
        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_neg_trait_impl(unsafe impl ! Foo for u32 {})

            Caused by:
                negative impls cannot be unsafe"#]]
    )
}

#[test]
fn unsafe_trait_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                unsafe trait Foo {}
                impl Foo for u32 {}
            }
        ]
        
        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(impl Foo for u32 { })

            Caused by:
                the trait `Foo` requires an `unsafe impl` declaration"#]]
    )
}

#[test]
fn safe_trait_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                trait Foo {}
                unsafe impl Foo for u32 {}
            }
        ]
        
        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(unsafe impl Foo for u32 { })

            Caused by:
                implementing the trait `Foo` is not unsafe"#]]
    )
}