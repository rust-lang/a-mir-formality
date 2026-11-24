//! `Sized` is a trait that can be written in bounds. It's proven by built-in rules and
//! users can't implement it.
//!
//! rustc gives type parameters an implicit `Sized` bound, so some programs here pass
//! rustc but fail in formality.

use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn sized_bound_is_provable() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl Needs<u32> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn fieldless_struct_is_sized() {
    FormalityTest::new(crates![crate Foo {
        struct S {}
        trait Needs<T> where T: Sized {}
        impl Needs<S> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn by_value_trait_param_with_sized_bound_compiles_with_rustc() {
    FormalityTest::new(crates![crate Foo {
        fn consume<T>(value: T) -> () where T: Sized {}

        trait Consumer<T> where T: Sized {
            fn consume_value(value: T) -> () {}
        }
        fn main() -> () { consume::<u32>(22_u32); }
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn shared_reference_is_sized_without_pointee_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<'a, T> Needs<&'a T> for u32 where T: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn mutable_reference_is_sized_without_pointee_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<'a, T> Needs<&'a mut T> for u32 where T: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn generic_sized_bound_is_forwarded() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<T> Needs<T> for u32 where T: Sized {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn generic_parameter_requires_explicit_sized_bound() {
    // Generated Rust still gives T an implicit Sized bound until #500 lands.
    // Formality must already require an explicit bound on this impl.
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<T> Needs<T> for u32 {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn user_defined_sized_impl_is_rejected() {
    FormalityTest::new(crates![crate Foo {
        struct S {}
        impl Sized for S {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0322]: explicit impls for the `Sized` trait are not permitted
         --> lib.rs
          |
        3 | impl Sized for S {}
          | ^^^^^^^^^^^^^^^^ impl of `Sized` not allowed

        For more information about this error, try `rustc --explain E0322`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "Sized" at (impls.rs) failed because
          explicit impls for the `Sized` trait are not permitted"#]]);
}

#[test]
fn user_defined_negative_sized_impl_is_rejected() {
    FormalityTest::new(crates![crate Foo {
        #![feature(negative_impls)]
        struct S {}
        impl !Sized for S {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0322]: explicit impls for the `Sized` trait are not permitted
         --> lib.rs
          |
        5 | impl !Sized for S {}
          | ^^^^^^^^^^^^^^^^^ impl of `Sized` not allowed

        For more information about this error, try `rustc --explain E0322`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "Sized" at (impls.rs) failed because
          explicit impls for the `Sized` trait are not permitted"#]]);
}

#[test]
fn raw_pointers_are_sized_without_pointee_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<T> Needs<*const T> for u32 {}
        impl<T> Needs<*mut T> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn unit_and_tuples_of_sized_types_are_sized() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl Needs<()> for u32 {}
        impl<T> Needs<(T, bool)> for u32 where T: Sized {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn tuple_requires_every_element_sized() {
    // In rustc, `(T, bool)` isn't even well-formed if `T` is unsized.
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<T> Needs<(T, bool)> for u32 {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn struct_with_generic_tail_is_sized_if_tail_is() {
    FormalityTest::new(crates![crate Foo {
        struct Tail<T> { prefix: u32, tail: T }
        trait Needs<T> where T: Sized {}
        impl<T> Needs<Tail<T>> for u32 where T: Sized {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn struct_with_generic_tail_requires_sized_tail() {
    // `Tail<T>` is only sized if `T` is.
    FormalityTest::new(crates![crate Foo {
        struct Tail<T> { prefix: u32, tail: T }
        trait Needs<T> where T: Sized {}
        impl<T> Needs<Tail<T>> for u32 {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn sized_non_final_field_is_accepted() {
    FormalityTest::new(crates![crate Foo {
        struct Good<T> where T: Sized { prefix: T, tail: u32 }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn unsized_non_final_field_is_rejected() {
    // Only the last field of a struct can be unsized.
    FormalityTest::new(crates![crate Foo {
        struct Bad<T> { prefix: T, tail: u32 }
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn enum_with_sized_fields_is_sized() {
    FormalityTest::new(crates![crate Foo {
        enum Maybe<T> where T: Sized { Just { value: T }, Nothing {} }
        trait Needs<T> where T: Sized {}
        impl<T> Needs<Maybe<T>> for u32 where T: Sized {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn unsized_enum_field_is_rejected() {
    // Enum fields must all be sized, even the last one.
    FormalityTest::new(crates![crate Foo {
        enum Maybe<T> { Nothing {}, Just { value: T } }
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn alias_is_sized_if_normalized_type_is() {
    FormalityTest::new(crates![crate Foo {
        trait Assoc { type Out: []; }
        impl Assoc for u32 { type Out = bool; }
        trait Needs<T> where T: Sized {}
        impl Needs<<u32 as Assoc>::Out> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn projection_non_final_field_with_sized_bound_is_accepted() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type A: []; }
        struct S<T> where T: Tr, <T as Tr>::A: Sized { a: <T as Tr>::A, b: u32 }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn blanket_impl_for_sized_types_overlaps_with_impl_for_struct() {
    // `S` is sized, so both impls apply to it.
    FormalityTest::new(crates![crate Foo {
        trait Foo {}
        struct S {}
        impl<T> Foo for T where T: Sized {}
        impl Foo for S {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0119]: conflicting implementations of trait `Foo` for type `S`
         --> lib.rs
          |
        5 | impl<T00> Foo for T00 where T00: Sized {}
          | -------------------------------------- first implementation here
        6 |
        7 | impl Foo for S {}
          | ^^^^^^^^^^^^^^ conflicting implementation for `S`

        For more information about this error, try `rustc --explain E0119`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "different trait" at (coherence.rs) failed because
          condition evaluated to false: `impl_a.trait_id() != impl_b.trait_id()`

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: ! Sized(!ty_0), via: !ty_0 = S, assumptions: {!ty_0 = S, Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: ! Sized(!ty_0), via: Sized(!ty_0), assumptions: {!ty_0 = S, Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "negative impl" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`

        the rule "not goal" at (coherence.rs) failed because
          failed to prove {!ty_1 = S, Sized(!ty_1)} given {}, got [Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }]

        the rule "same impl" at (coherence.rs) failed because
          condition evaluated to false: `impl_a == impl_b`
            impl_a = impl <ty> Foo for ^ty0_0 where ^ty0_0 : Sized { }
            impl_b = impl Foo for S { }"#]]);
}

#[test]
fn struct_tail_is_checked_after_substitution() {
    // `Pair<u32, X>` ends in `X`, which may be unsized.
    FormalityTest::new(crates![crate Foo {
        struct Pair<A, B> where A: Sized { first: A, last: B }
        trait Needs<T> where T: Sized {}
        impl<X> Needs<Pair<u32, X>> for u32 {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn struct_tail_is_followed_through_nested_structs() {
    // `Outer<T>` ends in `Tail<T>`, which ends in `T`.
    FormalityTest::new(crates![crate Foo {
        struct Tail<T> { prefix: u32, tail: T }
        struct Outer<T> { inner: Tail<T> }
        trait Needs<T> where T: Sized {}
        impl<T> Needs<Outer<T>> for u32 {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn alias_is_not_sized_if_normalized_type_may_be_unsized() {
    // `<T as Assoc>::Out` normalizes to `T`, which may be unsized.
    FormalityTest::new(crates![crate Foo {
        struct W<T> { t: T }
        trait Assoc { type Out: []; }
        impl<T> Assoc for T { type Out = T; }
        trait Needs<T> where T: Sized {}
        impl<T> Needs<<T as Assoc>::Out> for W<T> {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn recursive_struct_is_not_sized() {
    // `Sized` is inductive, so the cycle through `next` doesn't prove `List: Sized`.
    // rustc rejects the struct itself (E0072), which formality doesn't check.
    FormalityTest::new(crates![crate Foo {
        struct List { next: List }
        trait Needs<T> where T: Sized {}
        impl Needs<List> for u32 {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0072]: recursive type `List` has infinite size
         --> lib.rs
          |
        1 | pub struct List {
          | ^^^^^^^^^^^^^^^
        2 |     pub next: List,
          |               ---- recursive without indirection
          |
        help: insert some indirection (e.g., a `Box`, `Rc`, or `&`) to break the cycle
          |
        2 |     pub next: Box<List>,
          |               ++++    +

        For more information about this error, try `rustc --explain E0072`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn call_with_sized_struct_argument() {
    FormalityTest::new(crates![crate Foo {
        struct Tail<T> { prefix: u32, tail: T }
        fn consume<T>(value: T) -> () where T: Sized {}
        fn forward<T>(value: Tail<T>) -> () where T: Sized { consume::<Tail<T>>(value); }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn call_requires_sized_struct_argument() {
    FormalityTest::new(crates![crate Foo {
        struct Tail<T> { prefix: u32, tail: T }
        fn consume<T>(value: T) -> () where T: Sized {}
        fn forward<T>(value: Tail<T>) -> () { consume::<Tail<T>>(value); }
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_sized.rs:12:1: no applicable rules for prove_sized { ty: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn associated_sized_bound_allows_nonfinal_struct_field() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type A: [Sized]; }
        struct S<T> where T: Tr { first: <T as Tr>::A, last: u32 }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn associated_sized_bound_allows_enum_field() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type A: [Sized]; }
        enum E<T> where T: Tr { Some { value: <T as Tr>::A }, None {} }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn associated_sized_bound_requires_trait_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type A: [Sized]; }
        struct S<T> { first: <T as Tr>::A, last: u32 }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0277]: the trait bound `T00: Tr` is not satisfied
         --> lib.rs
          |
        6 |     pub first: <T00 as Tr>::A,
          |                ^^^^^^^^^^^^^^ the trait `Tr` is not implemented for `T00`
          |
        help: consider restricting type parameter `T00` with trait `Tr`
          |
        5 | pub struct S<T00: Tr> {
          |                 ++++

        For more information about this error, try `rustc --explain E0277`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn gat_sized_bound_allows_nonfinal_struct_field() {
    FormalityTest::new(crates![crate Foo {
        trait Bar {}
        trait Tr<V> { type A<U>: [Sized] where U: Bar; }
        struct S<T, V, U> where T: Tr<V>, U: Bar { first: <T as Tr<V>>::A<U>, last: u32 }
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn gat_sized_bound_requires_gat_where_clauses() {
    FormalityTest::new(crates![crate Foo {
        trait Bar {}
        trait Tr<V> { type A<U>: [Sized] where U: Bar; }
        struct S<T, V, U> where T: Tr<V> { first: <T as Tr<V>>::A<U>, last: u32 }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0277]: the trait bound `T02: Bar` is not satisfied
          --> lib.rs
           |
        13 |     pub first: <T00 as Tr<T01>>::A<T02>,
           |                ^^^^^^^^^^^^^^^^^^^^^^^^ the trait `Bar` is not implemented for `T02`
           |
        note: required by a bound in `Tr::A`
          --> lib.rs
           |
         4 |     type A<T10>: Sized
           |          - required by a bound in this associated type
         5 |     where
         6 |         T10: Bar;
           |              ^^^ required by this bound in `Tr::A`
        help: consider further restricting type parameter `T02` with trait `Bar`
           |
        11 |     T00: Tr<T01>, T02: Bar
           |                   ++++++++

        For more information about this error, try `rustc --explain E0277`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Bar(!ty_2), via: Tr(!ty_0, !ty_1), assumptions: {Tr(!ty_0, !ty_1)}, env: Env { variables: [!ty_0, !ty_1, !ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Sized(<!ty_0 as Tr<!ty_1>>::A<!ty_2>), via: Tr(!ty_0, !ty_1), assumptions: {Tr(!ty_0, !ty_1)}, env: Env { variables: [!ty_0, !ty_1, !ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: <!ty_0 as Tr<!ty_1>>::A<!ty_2>, via: Tr(!ty_0, !ty_1), assumptions: {Tr(!ty_0, !ty_1)}, env: Env { variables: [!ty_0, !ty_1, !ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn nested_struct_sized_bound_executes() {
    FormalityTest::new(crates![crate Foo {
        struct Tail<T> { prefix: u32, tail: T }
        struct Outer<T> { inner: Tail<T> }
        fn consume<T>(value: T) -> u32 where T: Sized { return 7_u32; }
        fn main() -> () {
            println!(consume::<Outer<u32>>(Outer::<u32> {
                inner: Tail::<u32> { prefix: 1_u32, tail: 2_u32 }
            }));
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .ok();
}
