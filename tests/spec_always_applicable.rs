//! `always_applicable trait Foo` (`#![feature(spec_always_applicable)]`,
//! rustc's `#[rustc_specialization_trait]`): every impl of `Foo` is checked
//! to apply regardless of lifetimes, so a bound on `Foo` needs no `may_spec`.
//! The use-site half of the mode is covered by `tests/may_spec.rs`.

#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};
use formality_macros::test;

/// A bound on an `always_applicable` trait is decided at codegen, per
/// monomorphization, with no `may_spec`: its impls do not depend on
/// lifetimes, so erasing them loses nothing.
#[test]
fn always_applicable_trait_needs_no_may_spec() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        always_applicable trait Foo {}
        impl Foo for u32 {}
        impl<'a> Foo for Tag<'a> {}
        fn spec<T>() -> () {
            if impls T: Foo { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
            spec::<Tag<'static>>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: ok, prints "1\n2\n1\n"
    "#]])
}

/// Impls may have where-clauses on other `always_applicable` traits.
#[test]
fn always_applicable_trait_impl_bound_on_always_applicable_trait() {
    FormalityTest::new(crates![crate foo {
        struct Wrapper<T> {}
        always_applicable trait Foo {}
        always_applicable trait Inner {}
        impl Inner for u32 {}
        impl<T> Foo for Wrapper<T> where T: Inner {}
        fn spec<T>() -> () {
            if impls Wrapper<T>: Foo { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: ok, prints "1\n2\n"
    "#]])
}

/// The declaration needs `spec_always_applicable`: rejected in every other
/// mode.
#[test]
fn always_applicable_trait_requires_feature() {
    FormalityTest::new(crates![crate foo {
        always_applicable trait Foo {}
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: ok
    "#]])
}

/// `impl Foo for Tag<'static>` applies only for `'static`: rejected.
#[test]
fn always_applicable_trait_rejects_static_impl() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        always_applicable trait Foo {}
        impl Foo for Tag<'static> {}
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: err:
            the rule "always applicable impl" at (specialization.rs) failed because
              condition evaluated to false: `!mentions_static(&header)`
                &header = [Tag<' static>]
    "#]])
}

/// `impl<'a> Foo for Pair<'a, 'a>` applies only if the two lifetimes are
/// equal: rejected.
#[test]
fn always_applicable_trait_rejects_repeated_lifetime() {
    FormalityTest::new(crates![crate foo {
        struct Pair<'a, 'b> {}
        always_applicable trait Foo {}
        impl<'a> Foo for Pair<'a, 'a> {}
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: err:
            the rule "always applicable impl" at (specialization.rs) failed because
              condition evaluated to false: `repeated.is_none()`
                repeated = Some(!lt_1)
    "#]])
}

/// `impl<T> Foo for Pair<T, T>` applies only if the two types are equal,
/// which depends on their lifetimes: rejected.
#[test]
fn always_applicable_trait_rejects_repeated_type() {
    FormalityTest::new(crates![crate foo {
        struct Pair<T, U> {}
        always_applicable trait Foo {}
        impl<T> Foo for Pair<T, T> {}
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: err:
            the rule "always applicable impl" at (specialization.rs) failed because
              condition evaluated to false: `repeated.is_none()`
                repeated = Some(!ty_1)
    "#]])
}

/// `where 'a: 'static` makes the impl depend on `'a`: rejected.
#[test]
fn always_applicable_trait_rejects_lifetime_where_clause() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        always_applicable trait Foo {}
        impl<'a> Foo for Tag<'a> where 'a: 'static {}
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: err:
            the rule "closed" at (specialization.rs) failed because
              condition evaluated to false: `wc.free_variables().is_empty()`
    "#]])
}

/// `where T: Bar` with `Bar` an ordinary trait, whose impls may depend on
/// lifetimes: rejected.
#[test]
fn always_applicable_trait_rejects_bound_on_ordinary_trait() {
    FormalityTest::new(crates![crate foo {
        struct Wrapper<T> {}
        trait Bar {}
        always_applicable trait Foo {}
        impl<T> Foo for Wrapper<T> where T: Bar {}
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*enabled`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: err:
            the rule "bound on an always applicable trait" at (specialization.rs) failed because
              condition evaluated to false: `*always_applicable`

            the rule "closed" at (specialization.rs) failed because
              condition evaluated to false: `wc.free_variables().is_empty()`
    "#]])
}
