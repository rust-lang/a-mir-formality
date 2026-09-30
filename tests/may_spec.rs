//! Branch specialization (`#![feature(branch_specialization)]`): `if impls`
//! and `may_spec` bounds. See the book chapter for the rules. Every behavior
//! test runs its program under every mode (`FormalityTest::spec_modes`).
//!
//! (`Tag<'a>` structs carry a lifetime without a borrow because codegen
//! cannot yet execute programs with live loans.)

#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};
use formality_macros::test;

// ---------------------------------------------------------------------------
// The feature gate, and the form and well-formedness of the bounds
// ---------------------------------------------------------------------------

#[test]
fn if_impls_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn main() -> () {
            if impls u32: Bar { println!(1_u32); }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// The feature gate is required wherever `may_spec` appears: on impls...
#[test]
fn may_spec_requires_feature_gate_on_impl() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Baz {}
        impl<T> Baz for T where may_spec(T: Bar) {}
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// ...and on ADTs...
#[test]
fn may_spec_requires_feature_gate_on_struct() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        struct S<T> where may_spec(T: Bar) {}
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate_under_for_all() {
    FormalityTest::new(crates![crate foo {
        trait Tr<'a> {}
        fn spec<T>() -> () where for<'a> may_spec(T: Tr<'a>) { }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate_on_associated_type() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Tr {
            type Out : [] where may_spec(Self: Bar);
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// ...and for an `if impls` nested inside other statements.
#[test]
fn if_impls_nested_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn main() -> () {
            'a: loop {
                if impls u32: Bar { println!(1_u32); } else { println!(2_u32); }
                break 'a;
            }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// Only a trait bound parses as the bound.
#[test]
fn may_spec_requires_trait_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        fn spec<'a, T>() -> () where may_spec(T: 'a) { }
    }])
    .err(expect_test::expect![[r#"
        × TraitId expected
           ╭─[1:1]
         1 │ [crate foo
           · ▲▲▲
           · ││╰── while parsing Crate
           · │╰── while parsing Vec
           · ╰── while parsing Crates
         2 │ {
         3 │     #![feature(branch_specialization)] fn spec<'a, T>() -> () where
           ·                                        ▲▲     ▲      ▲
           ·                                        ││     │      ╰── while parsing FnBoundData
           ·                                        ││     ╰── while parsing Binder
           ·                                        │╰── while parsing Fn
           ·                                        ╰── while parsing CrateItem
         4 │     may_spec(T: 'a) {}
           ·     ▲           ▲▲
           ·     │           │╰── TraitId expected
           ·     │           ╰── while parsing TraitId
           ·     ╰── while parsing WhereClause
         5 │ }]
           ╰────"#]])
}

#[test]
fn if_impls_requires_trait_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        fn spec<'a, T>() -> () {
            if impls T: 'a { } else { }
        }
    }])
    .err(expect_test::expect![[r#"
        × TraitId expected
           ╭─[1:1]
         1 │ [crate foo
           · ▲▲▲
           · ││╰── while parsing Crate
           · │╰── while parsing Vec
           · ╰── while parsing Crates
         2 │ {
         3 │     #![feature(branch_specialization)] fn spec<'a, T>() -> ()
           ·                                        ▲▲     ▲      ▲
           ·                                        ││     │      ╰── while parsing FnBoundData
           ·                                        ││     ╰── while parsing Binder
           ·                                        │╰── while parsing Fn
           ·                                        ╰── while parsing CrateItem
         4 │     { if impls T: 'a {} else {} }
           ·     ▲▲▲▲       ▲  ▲▲
           ·     ││││       │  │╰── TraitId expected
           ·     ││││       │  ╰── while parsing TraitId
           ·     ││││       ╰── while parsing IfImplsBound
           ·     │││╰── while parsing Stmt
           ·     ││╰── while parsing Block
           ·     │╰── while parsing FnBody
           ·     ╰── while parsing MaybeFnBody
         5 │ }]
           ╰────"#]])
}

/// `may_spec` bounds: on a trait bound; `for<..>` outside, as on any
/// where-clause.
#[test]
fn may_spec_accepted() {
    FormalityTest::new(crates![crate foo {
        trait Bar<'a> {}
        fn spec<T>() -> () where may_spec(T: Bar<'static>), for<'a> may_spec(T: Bar<'a>) { }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// `may_spec(T: Sub)` does not assume `T: Sub`, so `T: Super` is not
/// required...
#[test]
fn may_spec_does_not_require_supertraits() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Sub) { }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// ...but the parameters of the bound must still be well-formed.
#[test]
fn may_spec_requires_well_formed_parameters() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Baz {}
        struct S<T> where T: Bar {}
        fn spec<T>() -> () where may_spec(S<T>: Baz) { }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(S<!ty_0>), via: @ may_spec(Baz(S<!ty_0>)), assumptions: {@ may_spec(Baz(S<!ty_0>))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(Baz(S<!ty_0>)), assumptions: {@ may_spec(Baz(S<!ty_0>))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

/// Likewise for the bound of an `if impls`.
#[test]
fn if_impls_requires_well_formed_parameters() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Baz {}
        struct S<T> where T: Bar {}
        fn main() -> () {
            if impls S<u32>: Baz { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

// ---------------------------------------------------------------------------
// `may_spec` at call sites: the caller decides
// ---------------------------------------------------------------------------

/// Concrete callers decide on the spot: `u32: Bar` holds; `i32: Bar` is
/// closed and unprovable.
#[test]
fn may_spec_concrete_caller_decides() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// A generic caller cannot decide `T: Bar` and so may not call `spec::<T>`...
#[test]
fn may_spec_generic_caller_without_bound() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () {
            spec::<T>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

/// ...unless it knows `T: Bar`...
#[test]
fn may_spec_generic_caller_with_positive_bound() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () where T: Bar {
            spec::<T>();
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// ...or defers with its own `may_spec`.
#[test]
fn may_spec_generic_caller_with_may_spec_bound() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () where may_spec(T: Bar) {
            spec::<T>();
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// `may_spec(T: Sub)` does not decide `T: Super` (see the book, "What a
/// `may_spec` bound decides"): `caller` must declare `may_spec(T: Super)`.
#[test]
fn may_spec_subtrait_bound_does_not_decide_supertrait() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Super) { }
        fn caller<T>() -> () where may_spec(T: Sub) {
            spec::<T>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ may_spec(Super(!ty_0)), via: @ may_spec(Sub(!ty_0)), assumptions: {@ may_spec(Sub(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Super(!ty_0), via: @ may_spec(Sub(!ty_0)), assumptions: {@ may_spec(Sub(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(Sub(!ty_0)), assumptions: {@ may_spec(Sub(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(Sub(!ty_0))}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }
    "#]])
}

/// Nor does `may_spec(T: Super)` decide `T: Sub`.
#[test]
fn may_spec_supertrait_bound_does_not_decide_subtrait() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Sub) { }
        fn caller<T>() -> () where may_spec(T: Super) {
            spec::<T>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ may_spec(Sub(!ty_0)), via: @ may_spec(Super(!ty_0)), assumptions: {@ may_spec(Super(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(Super(!ty_0)), assumptions: {@ may_spec(Super(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(Super(!ty_0))}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }
    "#]])
}

/// With two `may_spec` bounds in scope, each decides the bound it names.
#[test]
fn may_spec_two_bounds() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Baz {}
        fn spec<T>() -> () where may_spec(T: Bar), may_spec(T: Baz) { }
        fn caller<T>() -> () where may_spec(T: Baz), may_spec(T: Bar) {
            spec::<T>();
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// A bound over a generic type is never decided by a failed search. We could,
/// in theory, be certain here because this impl cannot be added downstream. But,
/// for now we require that caller must declare `may_spec(Wrapper<T>: Bar)` itself.
#[test]
fn may_spec_generic_wrapper_is_undecided() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        struct Wrapper<T> { value: T }
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () {
            spec::<Wrapper<T>>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

/// A negative impl is never consulted: `T: Bar` stays undecided, since a
/// downstream type may implement `Bar`.
#[test]
fn may_spec_negative_impl_does_not_decide_generic() {
    FormalityTest::new(crates![crate foo {
        #![feature(negative_impls)]
        trait Bar {}
        impl<T> !Bar for T {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () {
            spec::<T>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

/// A bound under `for<T>` is not closed: a failed search does not decide it,
/// since a downstream crate may add an impl.
#[test]
fn may_spec_non_lifetime_binder_is_undecided() {
    FormalityTest::new(crates![crate foo {
        #![feature(non_lifetime_binders)]
        trait Static {}
        fn spec() -> () where for<T> may_spec(T: Static) { }
        fn caller() -> () {
            spec();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`

            the rule "local" at (nll.rs) failed because
              unknown local variable `spec`
    "#]])
}

/// A downstream crate decides using its own types and impls.
#[test]
fn may_spec_decided_downstream() {
    FormalityTest::new(crates![
        crate upstream {
            trait Bar {}
            fn spec<T>() -> () where may_spec(T: Bar) { }
        },
        crate downstream {
            struct Yes {}
            struct No {}
            impl Bar for Yes {}
            fn main() -> () {
                spec::<Yes>();
                spec::<No>();
            }
        }
    ])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

// ---------------------------------------------------------------------------
// `if impls` on closed types: decided locally
// ---------------------------------------------------------------------------

#[test]
fn if_impls_concrete_holds() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn main() -> () {
            if impls u32: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
    "#]])
}

#[test]
fn if_impls_concrete_does_not_hold() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn main() -> () {
            if impls i32: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "2\n"
    "#]])
}

/// A local type with no impl: the bound is closed and not provable, so it
/// does not hold and the else branch is taken.
#[test]
fn if_impls_local_type_does_not_hold() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        struct Local {}
        fn main() -> () {
            if impls Local: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "2\n"
    "#]])
}

// ---------------------------------------------------------------------------
// `if impls` on generics: decided by the callers, evaluated at codegen
// ---------------------------------------------------------------------------

/// Without `may_spec(T: Bar)`, `T: Bar` is undecided inside the function.
#[test]
fn if_impls_generic_without_may_spec() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

/// With `may_spec(T: Bar)`, each caller decides, and each monomorphization
/// takes its own branch.
#[test]
fn if_impls_generic_with_may_spec() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n2\n"
    "#]])
}

/// A blanket impl makes `T: Bar` hold outright: no `may_spec` needed.
#[test]
fn if_impls_blanket_impl_holds() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl<T> Bar for T {}
        fn spec<T>() -> () {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<u32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
    "#]])
}

/// A chain of generic callers: `caller` defers `T: Bar` to its own callers.
#[test]
fn if_impls_decided_through_generic_caller() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn caller<T>() -> () where may_spec(T: Bar) {
            spec::<T>();
        }
        fn main() -> () {
            caller::<i32>();
            caller::<u32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "2\n1\n"
    "#]])
}

/// A downstream crate decides using its own types and impls.
#[test]
fn if_impls_decided_downstream() {
    FormalityTest::new(crates![
        crate upstream {
            trait Bar {}
            fn spec<T>() -> () where may_spec(T: Bar) {
                if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
            }
        },
        crate downstream {
            struct Yes {}
            struct No {}
            impl Bar for Yes {}
            fn main() -> () {
                spec::<Yes>();
                spec::<No>();
            }
        }
    ])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n2\n"
    "#]])
}

// ---------------------------------------------------------------------------
// Assumptions inside the branches
// ---------------------------------------------------------------------------

/// Inside the then-branch, `T: Bar` is assumed, so a function requiring it
/// may be called; in the else-branch it may not.
#[test]
fn if_impls_then_branch_assumes_bound() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn needs_bar<T>() -> () where T: Bar { }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { needs_bar::<T>(); }
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

#[test]
fn if_impls_else_branch_does_not_assume_bound() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn needs_bar<T>() -> () where T: Bar { }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { } else { needs_bar::<T>(); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(Bar(!ty_0)), assumptions: {@ may_spec(Bar(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}

/// The else-branch assumes nothing new, but the `may_spec` bound still
/// decides a nested use there.
#[test]
fn if_impls_else_branch_nested_call() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        impl Bar for u32 {}
        fn inner<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(3_u32); } else { inner::<T>(); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "3\n2\n"
    "#]])
}

/// Decided by the caller: `may_spec`, and `main` passes `'static`. Codegen
/// erases that `'static` like any lifetime, and still takes the then-branch.
#[test]
fn if_impls_static_argument_is_erased_at_codegen() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait Static {}
        impl Static for Tag<'static> {}
        fn spec<'a>(t: Tag<'a>) -> () where may_spec(Tag<'a>: Static) {
            if impls Tag<'a>: Static { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<'static>(Tag::<'static> {});
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
    "#]])
}

/// `for<'a> Tag<'static>: Tr<'a>` holds, as `'static: 'a`. Codegen erases the
/// `'static` and agrees: an erased lifetime outlives every lifetime.
#[test]
fn if_impls_higher_ranked_bound_holds_after_erasure() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'b> {}
        trait Tr<'a> {}
        impl<'a, 'b> Tr<'a> for Tag<'b> where 'b: 'a {}
        fn main() -> () {
            if impls for<'a> Tag<'static>: Tr<'a> { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
    "#]])
}

// ---------------------------------------------------------------------------
// The exploit: a borrow of a local promoted to `'static` in the then-branch
// ---------------------------------------------------------------------------

/// `promote` is sound for the one type that implements `Static`,
/// `&'static u32`. After erasure `&'erased u32: Static` holds whatever the
/// lifetime was, so codegen alone would run the then-branch for `main`'s
/// borrow of `local` and hand out a `&'static u32` to it. That cannot
/// happen: `launder` itself cannot decide the bound (`'a: 'static` is not
/// known).
#[test]
fn exploit_local_borrow_promoted_to_static() {
    FormalityTest::new(crates![crate foo {
        trait Static {}
        impl Static for &'static u32 {}
        fn promote<'b>(y: &'b u32) -> &'static u32 where &'b u32: Static { trusted }
        fn launder<'a>(x: &'a u32) -> u32 {
            if impls &'a u32: Static {
                let s: &'static u32 = promote::<'a>(x);
                return *s;
            } else {
                return 0_u32;
            }
        }
        fn main() -> () {
            exists<'x> {
                let local: u32 = 22_u32;
                let y: &'x u32 = &'x local;
                let v: u32 = launder::<'x>(y);
                println!(v);
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!prove_for_some_lifetimes(&decls, &env, &assumptions, &goal).is_proven()`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `leaves_no_region_constraint(&env, &c)`
                &env = Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true }
                &c = Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [!lt_0 : ' static], allow_pending_outlives: true }, known_true: true, substitution: {} }

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `leaves_no_region_constraint(&env, &c)`
                &env = Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true }
                &c = Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [' static : !lt_0, !lt_0 : ' static], allow_pending_outlives: true }, known_true: true, substitution: {} }
    "#]])
}

/// With `may_spec`, the decision is `main`'s, which cannot make it either:
/// "yes" needs `'x: 'static`, and `'x` borrows `local`.
#[test]
fn exploit_local_borrow_promoted_to_static_with_may_spec() {
    FormalityTest::new(crates![crate foo {
        trait Static {}
        impl Static for &'static u32 {}
        fn promote<'b>(y: &'b u32) -> &'static u32 where &'b u32: Static { trusted }
        fn launder<'a>(x: &'a u32) -> u32 where may_spec(&'a u32: Static) {
            if impls &'a u32: Static {
                let s: &'static u32 = promote::<'a>(x);
                return *s;
            } else {
                return 0_u32;
            }
        }
        fn main() -> () {
            exists<'x> {
                let local: u32 = 22_u32;
                let y: &'x u32 = &'x local;
                let v: u32 = launder::<'x>(y);
                println!(v);
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ may_spec(Static(&?lt_0 u32)), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!prove_for_some_lifetimes(&decls, &env, &assumptions, &goal).is_proven()`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `leaves_no_region_constraint(&env, &c)`
                &env = Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true }
                &c = Constraints { env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {?lt_0 => ' static} }
    "#]])
}
