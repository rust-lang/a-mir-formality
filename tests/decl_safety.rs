#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};

#[test]
fn unsafe_trait() {
    FormalityTest::new(crates![crate baguette {
        unsafe trait Foo {}
        unsafe impl Foo for u32 {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

#[test]
fn safe_trait() {
    FormalityTest::new(crates![crate baguette {
        safe trait Foo {}
        safe impl Foo for u32 {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

#[test]
fn unsafe_trait_negative_impl() {
    FormalityTest::new(crates![crate baguette {
        #![feature(negative_impls)]
        unsafe trait Foo {}
        impl !Foo for u32 {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

#[test]
fn unsafe_trait_negative_impl_mismatch() {
    FormalityTest::new(crates![crate baguette {
        #![feature(negative_impls)]
        unsafe trait Foo {}
        unsafe impl !Foo for u32 {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0198]: negative impls cannot be unsafe
         --> lib.rs
          |
        5 | unsafe impl !Foo for u32 {}
          | ------      -^^^
          | |           |
          | |           negative because of this
          | unsafe because of this

        For more information about this error, try `rustc --explain E0198`.
        error: could not compile `baguette` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "check_neg_trait_impl" at (impls.rs) failed because
          negative impls cannot be unsafe"#]])
}

#[test]
fn safe_trait_negative_impl_mismatch() {
    FormalityTest::new(crates![crate baguette {
        #![feature(negative_impls)]
        trait Foo {}
        unsafe impl !Foo for u32 {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0198]: negative impls cannot be unsafe
         --> lib.rs
          |
        5 | unsafe impl !Foo for u32 {}
          | ------      -^^^
          | |           |
          | |           negative because of this
          | unsafe because of this

        For more information about this error, try `rustc --explain E0198`.
        error: could not compile `baguette` (lib) due to 1 previous error
    "#]])
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
    .rustc_err(expect_test::expect![[r#"
        error[E0200]: the trait `Foo` requires an `unsafe impl` declaration
         --> lib.rs
          |
        3 | impl Foo for u32 {}
          | ^^^^^^^^^^^^^^^^
          |
          = note: the trait `Foo` enforces invariants that the compiler can't check. Review the trait documentation and make sure this implementation upholds those invariants before adding the `unsafe` keyword
        help: add `unsafe` to this trait implementation
          |
        3 | unsafe impl Foo for u32 {}
          | ++++++

        For more information about this error, try `rustc --explain E0200`.
        error: could not compile `baguette` (lib) due to 1 previous error
    "#]])
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
    .rustc_err(expect_test::expect![[r#"
        error[E0199]: implementing the trait `Foo` is not unsafe
         --> lib.rs
          |
        3 | unsafe impl Foo for u32 {}
          | ^^^^^^^^^^^^^^^^^^^^^^^
          |
        help: remove `unsafe` from this trait implementation
          |
        3 - unsafe impl Foo for u32 {}
        3 + impl Foo for u32 {}
          |

        For more information about this error, try `rustc --explain E0199`.
        error: could not compile `baguette` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
            the rule "safety matches" at (impls.rs) failed because
              condition evaluated to false: `trait_decl.safety == trait_impl.safety`"#]])
}
