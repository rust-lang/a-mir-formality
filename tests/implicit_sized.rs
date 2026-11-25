//! Regression cases for implicit `Sized` bounds in generated Rust.

use a_mir_formality::{crates, FormalityTest};

// FIXME(#557): The blanket impl should cover possibly-unsized `Self`.
#[test]
#[should_panic(expected = "required for `Self` to implement `Marker`")]
fn blanket_impl_covers_trait_self() {
    FormalityTest::new(crates![crate Foo {
        trait Marker {}
        impl<T> Marker for T {}
        struct NeedsMarker<T> where T: Marker { value: T }
        trait Factory { fn make() -> NeedsMarker<Self>; }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

// FIXME(#557): Expect `impl<T00: ?Sized>` so the negative impl covers unsized types.
#[test]
fn negative_blanket_impl_parameter() {
    formality_rust::assert_rust!(
        [crate Foo {
            #![feature(negative_impls)]
            trait Marker {}
            impl<T> !Marker for T {}
        }],
        expect_test::expect![[r#"
            #![feature(negative_impls)]

            pub trait Marker { }

            impl<T00> !Marker for T00 {}"#]]
    );
}

// FIXME(#557): Expect `T10: ?Sized` on both the method declaration and its definition.
#[test]
fn generic_parameter_on_trait_and_impl_methods() {
    formality_rust::assert_rust!(
        [crate Foo {
            trait Visit { fn visit<T>() -> (); }
            impl Visit for u32 { fn visit<T>() -> () {} }
        }],
        expect_test::expect![[r#"
            pub trait Visit {
                fn visit<T10>() -> ();
            }

            impl Visit for u32 {
                fn visit<T10>() -> () {}
            }"#]]
    );
}

// FIXME(#557): Calling `visit::<Self>()` should not require `Self: Sized`.
#[test]
#[should_panic(expected = "required by an implicit `Sized` bound in `visit`")]
fn function_accepts_possibly_unsized_type_argument() {
    FormalityTest::new(crates![crate Foo {
        fn visit<T>() -> () {}
        trait Visit { fn visit_self() -> () { visit::<Self>(); } }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

// FIXME(#557): Storing a reference should not require its referent to be sized.
#[test]
#[should_panic(expected = "required by an implicit `Sized` bound in `View`")]
fn enum_accepts_possibly_unsized_referent() {
    FormalityTest::new(crates![crate Foo {
        enum View<'a, T> where T: 'a { Ref { value: &'a T } }
        trait Borrow { fn view<'a>() -> View<'a, Self> where Self: 'a; }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

// FIXME(#557): Relax the GAT parameter in both the declaration and the definition.
#[test]
#[should_panic(expected = "required by an implicit `Sized` bound in `Family::Item`")]
fn associated_type_accepts_possibly_unsized_argument() {
    FormalityTest::new(crates![crate Foo {
        trait Family { type Item<T>: []; }
        impl Family for u32 { type Item<T> = u32; }
        trait UseFamily { fn item() -> <u32 as Family>::Item<Self>; }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}
