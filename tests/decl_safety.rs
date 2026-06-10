#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};

#[test]
fn unsafe_trait() {
    FormalityTest::new(crates![crate baguette {
        unsafe trait Foo {}
        unsafe impl Foo for u32 {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn safe_trait() {
    FormalityTest::new(crates![crate baguette {
        safe trait Foo {}
        safe impl Foo for u32 {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn unsafe_trait_negative_impl() {
    FormalityTest::new(crates![crate baguette {
        unsafe trait Foo {}
        impl !Foo for u32 {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn unsafe_trait_negative_impl_mismatch() {
    FormalityTest::new(crates![crate baguette {
        unsafe trait Foo {}
        unsafe impl !Foo for u32 {}
    }])
    .err(expect_test::expect![[r#"
        the rule "check_neg_trait_impl" at (impls.rs) failed because
          negative impls cannot be unsafe"#]])
}

#[test]
fn safe_trait_negative_impl_mismatch() {
    FormalityTest::new(crates![crate baguette {
        trait Foo {}
        unsafe impl !Foo for u32 {}
    }])
    .err(expect_test::expect![[r#"
        the rule "check_neg_trait_impl" at (impls.rs) failed because
          negative impls cannot be unsafe"#]])
}

#[test]
fn unsafe_trait_mismatch() {
    FormalityTest::new(crates![crate baguette {
        unsafe trait Foo {}
        impl Foo for u32 {}
    }])
    .err(expect_test::expect![[r#"
            the rule "safety matches" at (impls.rs) failed because
              condition evaluated to false: `trait_decl.safety == trait_impl.safety`"#]])
}

#[test]
fn safe_trait_mismatch() {
    FormalityTest::new(crates![crate baguette {
        trait Foo {}
        unsafe impl Foo for u32 {}
    }])
    .err(expect_test::expect![[r#"
            the rule "safety matches" at (impls.rs) failed because
              condition evaluated to false: `trait_decl.safety == trait_impl.safety`"#]])
}
