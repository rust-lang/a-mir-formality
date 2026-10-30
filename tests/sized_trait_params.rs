//! Type parameters of traits and structs must not carry rustc's implicit `Sized` bound.
//! Each program is checked with rustc (`rustc_ok`).

use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

// `T: Bar<Self>` needs `Bar`'s parameter to be `?Sized`, because `Self` is not `Sized` in a trait.
#[test]
fn trait_param_used_with_self() {
    FormalityTest::new(crates![crate Foo {
        trait Bar<T> {}
        trait Foo<T> where T: Bar<Self> {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

// A `?Sized` trait parameter used as an argument of a struct, which needs `?Sized` too.
#[test]
fn trait_param_used_in_struct_type() {
    FormalityTest::new(crates![crate Foo {
        struct Wrapper<T> {
            value: T,
        }

        trait Factory<T> {
            fn make() -> Wrapper<T>;
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

// A `?Sized` trait parameter used as a by-value fn parameter in a default method.
// A by-value parameter needs `T: Sized`, so the program says so explicitly (formality
// makes `Sized` explicit). The generated trait prints as `T: ?Sized + Sized`.
#[test]
fn trait_param_used_by_value_in_default_method() {
    FormalityTest::new(crates![crate Foo {
        fn consume<T>(value: T) -> () where T: Sized { trusted }

        trait Consumer<T> where T: Sized {
            fn consume_value(value: T) -> () { trusted }
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}
