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
          condition evaluated to false: `*enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*enabled`"#]])
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
          condition evaluated to false: `*enabled`"#]])
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
          condition evaluated to false: `*enabled`"#]])
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
          condition evaluated to false: `*enabled`"#]])
}

/// Only trait bounds (possibly under `for<..>`) parse as the bound.
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
           ·     ▲        ▲  ▲▲
           ·     │        │  │╰── TraitId expected
           ·     │        │  ╰── while parsing TraitId
           ·     │        ╰── while parsing MaySpecBound
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
           ·     ││││       ╰── while parsing MaySpecBound
           ·     │││╰── while parsing Stmt
           ·     ││╰── while parsing Block
           ·     │╰── while parsing FnBody
           ·     ╰── while parsing MaybeFnBody
         5 │ }]
           ╰────"#]])
}

/// `may_spec` bounds: on a trait bound, possibly under `for<..>`.
#[test]
fn may_spec_accepted() {
    FormalityTest::new(crates![crate foo {
        trait Bar<'a> {}
        fn spec<T>() -> () where may_spec(T: Bar<'static>), may_spec(for<'a> T: Bar<'a>) { }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ wf(S<!ty_0>), via: @ may_spec(S<!ty_0> : Baz), assumptions: {@ may_spec(S<!ty_0> : Baz)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(S<!ty_0> : Baz), assumptions: {@ may_spec(S<!ty_0> : Baz)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(!ty_0 : Super), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Sub(!ty_0)]
                &goal = Super(!ty_0)

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Super(!ty_0), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(!ty_0 : Sub), via: @ may_spec(!ty_0 : Super), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Super(!ty_0)]
                &goal = Sub(!ty_0)

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(!ty_0 : Super), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Bar(Wrapper<!ty_0>), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: u32, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: ok, prints "1\n2\n"
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
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
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(!ty_0 : Bar), assumptions: {@ may_spec(!ty_0 : Bar)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
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
        commit-and-verify: as strict
        bail-on-regions: as strict
    "#]])
}

/// Without `may_spec`, nothing decides a nested use in the else-branch
/// either: the else-branch assumes nothing, in every mode.
#[test]
fn if_impls_else_branch_without_may_spec() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn needs_may_spec<T>() -> () where may_spec(T: Bar) { }
        fn spec<T>() -> () {
            if impls T: Bar { } else { needs_may_spec::<T>(); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
    "#]])
}

/// A `'static` in the generic arguments is erased at codegen like any
/// lifetime; the then-branch is still taken, as `main` decided.
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
        commit-and-verify: as strict
        bail-on-regions: ok, prints "2\n"
    "#]])
}

// ---------------------------------------------------------------------------
// Lifetimes: the modes
// ---------------------------------------------------------------------------

/// `Tag<'a>: Static` needs `'a: 'static`, and nothing in scope decides it.
#[test]
fn if_impls_lifetime_dependent() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait Static {}
        impl Static for Tag<'static> {}
        fn spec<'a>(t: Tag<'a>) -> () {
            if impls Tag<'a>: Static { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<'static>(Tag::<'static> {});
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Static(Tag<!lt_0>), assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            crates/formality-rust/src/check/borrow_check/outlives.rs:58:1: no applicable rules for can_outlive { param_a: !lt_1, param_b: ' static, assumptions: {}, env: TypeckEnv { env: Env { variables: [!lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(()) }, outlives: {pending_outlives(!lt_1, ' static)} }
        bail-on-regions: ok, prints "2\n"
    "#]])
}

/// The same, decided by the signature: `where 'a: 'static`.
#[test]
fn if_impls_lifetime_dependent_implied() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait Static {}
        impl Static for Tag<'static> {}
        fn spec<'a>(t: Tag<'a>) -> () where 'a: 'static {
            if impls Tag<'a>: Static { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<'static>(Tag::<'static> {});
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
        commit-and-verify: as strict
        bail-on-regions: ok, prints "2\n"
    "#]])
}

/// `impl<'a> AnyLt for Tag<'a>` holds for every `'a`.
#[test]
fn if_impls_lifetime_independent() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait AnyLt {}
        impl<'a> AnyLt for Tag<'a> {}
        fn spec<'a>(t: Tag<'a>) -> () {
            if impls Tag<'a>: AnyLt { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            exists<'a> {
                spec::<'a>(Tag::<'a> {});
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
    "#]])
}

/// Decided by the caller: `may_spec`, and `main` passes `'static`.
#[test]
fn may_spec_lifetime_dependent_delegated() {
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
        commit-and-verify: as strict
        bail-on-regions: ok, prints "2\n"
    "#]])
}

/// ...and `main` passes a free local region: nothing forces it to be
/// `'static`, and nothing forbids it.
#[test]
fn may_spec_lifetime_dependent_local_caller() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait Static {}
        impl Static for Tag<'static> {}
        fn spec<'a>(t: Tag<'a>) -> () where may_spec(Tag<'a>: Static) {
            if impls Tag<'a>: Static { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            exists<'a> {
                spec::<'a>(Tag::<'a> {});
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(Tag<?lt_0> : Static), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Static(Tag<?lt_0>), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: ok, prints "1\n"
        bail-on-regions: as strict
    "#]])
}

/// Decided locally, `'x: 'static` reduces through `'a: 'x` to
/// `'a: 'static` on the signature, which nothing entails: rejected. That
/// residue is what tells the author to add a `may_spec`.
#[test]
fn if_impls_local_region_residue_on_signature() {
    FormalityTest::new(crates![crate foo {
        trait Static {}
        impl Static for &'static u32 {}
        fn spec<'a>(x: &'a u32) -> () {
            exists<'x> {
                let y: &'x u32 = x;
                if impls &'x u32: Static { println!(1_u32); } else { println!(2_u32); }
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Static), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Static(&?lt_0 u32), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            crates/formality-rust/src/check/borrow_check/outlives.rs:58:1: no applicable rules for can_outlive { param_a: !lt_1, param_b: ' static, assumptions: {@ wf(?lt_2)}, env: TypeckEnv { env: Env { variables: [!lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(()) }, outlives: {pending_outlives(' static, ?lt_2), pending_outlives(!lt_1, ?lt_2), pending_outlives(?lt_2, ' static)} }
        bail-on-regions: ok
    "#]])
}

/// `'x` is local and unconstrained (no borrow, argument or return type
/// mentions it): nothing in scope entails `'x: 'static`, and nothing forbids
/// inference from choosing `'static` for it.
#[test]
fn if_impls_free_local_region() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait Static {}
        impl Static for Tag<'static> {}
        fn main() -> () {
            exists<'x> {
                if impls Tag<'x>: Static { println!(1_u32); } else { println!(2_u32); }
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(Tag<?lt_0> : Static), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/may_spec.rs:32:1: no applicable rules for decide_by_bound { goal: Static(Tag<?lt_0>), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: ok, prints "1\n"
        bail-on-regions: ok, prints "2\n"
    "#]])
}
