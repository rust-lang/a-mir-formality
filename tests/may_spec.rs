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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Sub(!ty_0)`

            crates/formality-rust/src/prove/may_spec.rs:83:1: no applicable rules for unify_bounds { a: Sub(!ty_0), b: Super(!ty_0), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Sub(!ty_0)]
                &goal = Super(!ty_0)

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Super(!ty_0), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
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

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Super(!ty_0)`

            crates/formality-rust/src/prove/may_spec.rs:83:1: no applicable rules for unify_bounds { a: Super(!ty_0), b: Sub(!ty_0), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Super(!ty_0)]
                &goal = Sub(!ty_0)

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(!ty_0 : Super), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
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
        always-applicable: as strict
    "#]])
}

/// Coherence decides `Wrapper<T>: Bar` as "no": no impl applies, and none
/// can be added downstream (`Wrapper<Local>` is covered) or upstream (both
/// are local).
#[test]
fn may_spec_generic_wrapper_decided_by_coherence() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        struct Wrapper<T> { value: T }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn caller<T>() -> () {
            spec::<Wrapper<T>>();
        }
        fn main() -> () {
            caller::<u32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "2\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// With `Bar` and `Wrapper` upstream, a minor release may add the impl:
/// undecided.
#[test]
fn may_spec_upstream_wrapper_is_undecided() {
    FormalityTest::new(crates![
        crate upstream {
            trait Bar {}
            struct Wrapper<T> { value: T }
            fn spec<T>() -> () where may_spec(T: Bar) { }
        },
        crate downstream {
            fn caller<T>() -> () {
                spec::<Wrapper<T>>();
            }
        }
    ])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Bar(Wrapper<!ty_0>), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// With `Bar` upstream and `Wrapper` local, nobody can add the impl:
/// decided.
#[test]
fn may_spec_upstream_trait_local_wrapper_decided_by_coherence() {
    FormalityTest::new(crates![
        crate upstream {
            trait Bar {}
            fn spec<T>() -> () where may_spec(T: Bar) { }
        },
        crate downstream {
            struct Wrapper<T> { value: T }
            fn caller<T>() -> () {
                spec::<Wrapper<T>>();
            }
        }
    ])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: u32, via: Bar(!ty_0), assumptions: {Bar(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: ok, prints "1\n2\n"
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
        always-applicable: as strict
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
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?ty_1], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
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
        always-applicable: err:
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(Tag<' static>), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Tag<!lt_0> = Tag<' static>, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<!lt_0>, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 = ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<' static>, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: ' static = !lt_0, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
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
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(Tag<!lt_0>), assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            crates/formality-rust/src/check/borrow_check/outlives.rs:58:1: no applicable rules for can_outlive { param_a: !lt_1, param_b: ' static, assumptions: {}, env: TypeckEnv { env: Env { variables: [!lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(()) }, outlives: {pending_outlives(!lt_1, ' static)} }
        bail-on-regions: ok, prints "2\n"
        always-applicable: as strict
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
        always-applicable: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(Tag<!lt_0> : Static), via: !lt_0 : ' static, assumptions: {!lt_0 : ' static}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(Tag<!lt_0>), assumptions: {!lt_0 : ' static}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Static(Tag<!lt_1>), via: !lt_0 : ' static, assumptions: {!lt_0 : ' static}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Tag<!lt_1> = Tag<' static>, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Tag<!lt_1> = Tag<' static>, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<!lt_1>, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<!lt_1>, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 = ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 = ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_1, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_1, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<' static>, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<' static>, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: ' static = !lt_1, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: ' static = !lt_1, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : ' static, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_1, via: !lt_0 : ' static, assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_1, via: Static(Tag<!lt_1>), assumptions: {!lt_0 : ' static, Static(Tag<!lt_1>)}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
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
        always-applicable: as strict
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
        always-applicable: err:
            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(Tag<' static>), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Tag<!lt_0> = Tag<' static>, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<!lt_0>, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 = ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<' static>, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: ' static = !lt_0, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: Static(Tag<!lt_0>), assumptions: {Static(Tag<!lt_0>)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
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

            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(Tag<?lt_0>), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: ok, prints "1\n"
        bail-on-regions: as strict
        always-applicable: as strict
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

            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(&?lt_0 u32), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            crates/formality-rust/src/check/borrow_check/outlives.rs:58:1: no applicable rules for can_outlive { param_a: !lt_1, param_b: ' static, assumptions: {@ wf(?lt_2)}, env: TypeckEnv { env: Env { variables: [!lt_1, ?lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(()) }, outlives: {pending_outlives(' static, ?lt_2), pending_outlives(!lt_1, ?lt_2), pending_outlives(?lt_2, ' static)} }
        bail-on-regions: ok
        always-applicable: as strict
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

            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: Static(Tag<?lt_0>), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: ok, prints "1\n"
        bail-on-regions: ok, prints "2\n"
        always-applicable: as strict
    "#]])
}

// ---------------------------------------------------------------------------
// Higher-ranked bounds
// ---------------------------------------------------------------------------

/// `may_spec(for<'a> T: Bar<'a>)` decides `if impls for<'b> T: Bar<'b>` (the
/// same bound). For `i32`, the bound is closed and unprovable at the call.
#[test]
fn if_impls_higher_ranked_with_may_spec() {
    FormalityTest::new(crates![crate foo {
        trait Bar<'a> {}
        impl<'a> Bar<'a> for u32 {}
        fn spec<T>() -> () where may_spec(for<'a> T: Bar<'a>) {
            if impls for<'b> T: Bar<'b> { println!(1_u32); } else { println!(2_u32); }
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
        always-applicable: as strict
    "#]])
}

/// A higher-ranked bound on a concrete type is decided on the spot, like any
/// other: `u32` implements `Bar<'a>` for every `'a`, `i32` for none.
#[test]
fn if_impls_higher_ranked_concrete() {
    FormalityTest::new(crates![crate foo {
        trait Bar<'a> {}
        impl<'a> Bar<'a> for u32 {}
        fn main() -> () {
            if impls for<'a> u32: Bar<'a> { println!(1_u32); } else { println!(2_u32); }
            if impls for<'a> i32: Bar<'a> { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n2\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// A higher-ranked bound is deferred through a generic caller like any
/// other.
#[test]
fn if_impls_higher_ranked_decided_through_generic_caller() {
    FormalityTest::new(crates![crate foo {
        trait Bar<'a> {}
        impl<'a> Bar<'a> for u32 {}
        fn spec<T>() -> () where may_spec(for<'a> T: Bar<'a>) {
            if impls for<'b> T: Bar<'b> { println!(1_u32); } else { println!(2_u32); }
        }
        fn caller<T>() -> () where may_spec(for<'a> T: Bar<'a>) {
            spec::<T>();
        }
        fn main() -> () {
            caller::<u32>();
            caller::<i32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n2\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// `may_spec(for<'a> T: Tr<'a>)` decides `T: Tr<'x>`: the caller's "yes" covers
/// every `'x`.
#[test]
fn may_spec_higher_ranked_bound_decides_instance() {
    FormalityTest::new(crates![crate foo {
        trait Tr<'a> {}
        impl<'a> Tr<'a> for u32 {}
        fn needs<'x, T>() -> () where T: Tr<'x> { }
        fn spec<'x, T>() -> () where may_spec(for<'a> T: Tr<'a>) {
            if impls T: Tr<'x> { needs::<'x, T>(); println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            exists<'x> {
                spec::<'x, u32>();
                spec::<'x, i32>();
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n2\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// ...also for a body-local `'x`: `'a := 'x` binds only the bound's own `'a`.
#[test]
fn may_spec_higher_ranked_bound_decides_instance_over_local_region() {
    FormalityTest::new(crates![crate foo {
        trait Tr<'a> {}
        impl<'a> Tr<'a> for u32 {}
        fn needs<'x, T>() -> () where T: Tr<'x> { }
        fn spec<T>() -> () where may_spec(for<'a> T: Tr<'a>) {
            exists<'x> {
                if impls T: Tr<'x> { needs::<'x, T>(); println!(1_u32); } else { println!(2_u32); }
            }
        }
        fn main() -> () {
            spec::<u32>();
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// A family that fails only for some lifetimes (`impl Tr<'static> for u32`)
/// is never decided "no": `main` cannot decide it (the constraint on `'a` is
/// left to the borrow checker, which cannot discharge it), so no "no" that
/// the erased instance would contradict (`u32: Tr<'erased>` holds) is ever
/// passed along.
#[test]
fn may_spec_higher_ranked_bound_lifetime_dependent_undecided() {
    FormalityTest::new(crates![crate foo {
        trait Tr<'a> {}
        impl Tr<'static> for u32 {}
        fn needs<'x, T>() -> () where T: Tr<'x> { }
        fn spec<'x, T>() -> () where may_spec(for<'a> T: Tr<'a>) {
            if impls T: Tr<'x> { needs::<'x, T>(); println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            exists<'x> {
                spec::<'x, u32>();
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(for <lt> u32 : Tr <^lt0_0>), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/may_spec.rs:34:1: no applicable rules for decide_by_bound { goal: for <lt> Tr(u32, ^lt0_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            crates/formality-rust/src/check/borrow_check/outlives.rs:58:1: no applicable rules for can_outlive { param_a: !lt_1, param_b: ' static, assumptions: {@ wf(?lt_1)}, env: TypeckEnv { env: Env { variables: [?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(()) }, outlives: {pending_outlives(!lt_1, ' static)} }
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// With a blanket impl generic over `'a`, `T: Tr<'x>` holds outright and the
/// family bound is not consulted.
#[test]
fn may_spec_higher_ranked_instance_blanket_impl() {
    FormalityTest::new(crates![crate foo {
        trait Tr<'a> {}
        impl<'a, T> Tr<'a> for T {}
        fn needs<'x, T>() -> () where T: Tr<'x> { }
        fn spec<'x, T>() -> () where may_spec(for<'a> T: Tr<'a>) {
            if impls T: Tr<'x> { needs::<'x, T>(); println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            exists<'x> {
                spec::<'x, u32>();
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: ok, prints "1\n"
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// The reverse is not decided: `main`'s "yes" for `Tag<'x>: Tr<'x>` says
/// nothing about the other lifetimes.
#[test]
fn may_spec_instance_bound_does_not_decide_higher_ranked() {
    FormalityTest::new(crates![crate foo {
        struct Tag<'a> {}
        trait Tr<'a> {}
        impl<'a> Tr<'a> for Tag<'a> {}
        fn needs_all<T>() -> () where for<'a> T: Tr<'a> { }
        fn spec<'x, T>() -> () where may_spec(T: Tr<'x>) {
            if impls for<'a> T: Tr<'a> { needs_all::<T>(); println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            exists<'x> {
                spec::<'x, Tag<'x>>();
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(for <lt> !ty_0 : Tr <^lt0_0>), via: @ may_spec(!ty_0 : Tr <!lt_1>), assumptions: {@ may_spec(!ty_0 : Tr <!lt_1>)}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Tr(!ty_0, !lt_1)`

            crates/formality-rust/src/prove/may_spec.rs:83:1: no applicable rules for unify_bounds { a: Tr(!ty_0, !lt_1), b: for <lt> Tr(!ty_0, ^lt0_0), assumptions: {@ may_spec(!ty_0 : Tr <!lt_1>)}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Tr(!ty_0, !lt_1)]
                &goal = for <lt> Tr(!ty_0, ^lt0_0)

            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [?lt_1, ?ty_2], bias: Completeness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Tr(!ty_0, !lt_2), via: @ may_spec(!ty_0 : Tr <!lt_1>), assumptions: {@ may_spec(!ty_0 : Tr <!lt_1>)}, env: Env { variables: [!lt_1, !ty_0, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !ty_0 = Tag<?lt_3>, via: @ may_spec(!ty_0 : Tr <!lt_2>), assumptions: {Tr(!ty_0, !lt_1), @ may_spec(!ty_0 : Tr <!lt_2>)}, env: Env { variables: [!lt_2, !ty_0, !lt_1, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !ty_0 = Tag<?lt_3>, via: Tr(!ty_0, !lt_1), assumptions: {Tr(!ty_0, !lt_1), @ may_spec(!ty_0 : Tr <!lt_2>)}, env: Env { variables: [!lt_2, !ty_0, !lt_1, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: @ may_spec(!ty_0 : Tr <!lt_2>), assumptions: {Tr(!ty_0, !lt_1), @ may_spec(!ty_0 : Tr <!lt_2>)}, env: Env { variables: [!lt_2, !ty_0, !lt_1, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Tr(!ty_0, !lt_1), assumptions: {Tr(!ty_0, !lt_1), @ may_spec(!ty_0 : Tr <!lt_2>)}, env: Env { variables: [!lt_2, !ty_0, !lt_1, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<?lt_3>, via: @ may_spec(!ty_0 : Tr <!lt_2>), assumptions: {Tr(!ty_0, !lt_1), @ may_spec(!ty_0 : Tr <!lt_2>)}, env: Env { variables: [!lt_2, !ty_0, !lt_1, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: Tag<?lt_3>, via: Tr(!ty_0, !lt_1), assumptions: {Tr(!ty_0, !lt_1), @ may_spec(!ty_0 : Tr <!lt_2>)}, env: Env { variables: [!lt_2, !ty_0, !lt_1, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
        commit-and-verify: as strict
        bail-on-regions: ok, prints "2\n"
        always-applicable: as strict
    "#]])
}

// ---------------------------------------------------------------------------
// Local regions: reduced to the signature by region inference
// ---------------------------------------------------------------------------

/// `may_spec(&'a u32: Static)` decides `&'x u32: Static` (`'a: 'x`) up to
/// `'x == 'a`, a constraint left to region inference.
#[test]
fn if_impls_local_region_via_bound() {
    FormalityTest::new(crates![crate foo {
        trait Static {}
        impl Static for &'static u32 {}
        fn spec<'a>(x: &'a u32) -> () where may_spec(&'a u32: Static) {
            exists<'x> {
                let y: &'x u32 = x;
                if impls &'x u32: Static { println!(1_u32); } else { println!(2_u32); }
            }
        }
        fn caller(r: &'static u32) -> () {
            spec::<'static>(r);
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Static), via: @ may_spec(&!lt_1 u32 : Static), assumptions: {@ wf(?lt_0), @ may_spec(&!lt_1 u32 : Static)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Static), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ may_spec(&!lt_1 u32 : Static)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Static(&!lt_1 u32)`

            the rule "bound unifies" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Static(&!lt_1 u32)]
                &goal = Static(&?lt_0 u32)

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: ok
        bail-on-regions: ok
        always-applicable: as strict
    "#]])
}

/// `&'x u32: Always` holds outright, so the bound `may_spec(&'a u32:
/// Always)`, which would match only up to `'x == 'a` (rejected by the
/// borrow checker: `y` borrows a local), is not consulted.
#[test]
fn if_impls_outright_with_bound_in_scope() {
    FormalityTest::new(crates![crate foo {
        trait Always {}
        impl<'a> Always for &'a u32 {}
        fn needs<'b>(y: &'b u32) -> () where &'b u32: Always { }
        fn spec<'a>(x: &'a u32) -> () where may_spec(&'a u32: Always) {
            exists<'x> {
                let local: u32 = 0_u32;
                let y: &'x u32 = &'x local;
                if impls &'x u32: Always { needs::<'x>(y); } else { }
            }
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// The book's example: `&'a u32: Super` is not the `Sub` bound, holds only
/// under `'a: 'static`, and is not "no". Were the `Sub` bound to decide it,
/// nothing would register `'a: 'static`, and codegen would take the
/// then-branch for the local that `main` passes (next test).
#[test]
fn may_spec_subtrait_bound_does_not_decide_supertrait_regions() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        trait Sub where Self: Super {}
        impl Super for u32 {}
        impl Sub for u32 {}
        impl Super for &'static u32 {}
        fn needs_super<T>(t: T) -> () where T: Super { }
        fn caller<'a>(x: &'a u32) -> () where may_spec(&'a u32: Sub) {
            if impls &'a u32: Super { needs_super::<&'a u32>(x); } else { }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&!lt_0 u32 : Super), via: @ may_spec(&!lt_0 u32 : Sub), assumptions: {@ may_spec(&!lt_0 u32 : Sub)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Sub(&!lt_0 u32)`

            crates/formality-rust/src/prove/may_spec.rs:83:1: no applicable rules for unify_bounds { a: Sub(&!lt_0 u32), b: Super(&!lt_0 u32), assumptions: {@ may_spec(&!lt_0 u32 : Sub)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Sub(&!lt_0 u32)]
                &goal = Super(&!lt_0 u32)

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            crates/formality-rust/src/check/borrow_check/outlives.rs:58:1: no applicable rules for can_outlive { param_a: !lt_1, param_b: ' static, assumptions: {@ may_spec(&!lt_1 u32 : Sub)}, env: TypeckEnv { env: Env { variables: [!lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(()) }, outlives: {pending_outlives(!lt_1, ' static)} }
        bail-on-regions: ok
        always-applicable: as strict
    "#]])
}

/// That `main`: `&'x u32: Sub` is closed and does not hold, decided "no"
/// even for a reference to a local. (`caller` has no `if impls` here.)
#[test]
fn may_spec_subtrait_bound_main_decides_no() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        trait Sub where Self: Super {}
        impl Super for u32 {}
        impl Sub for u32 {}
        impl Super for &'static u32 {}
        fn caller<'a>(x: &'a u32) -> () where may_spec(&'a u32: Sub) { }
        fn main() -> () {
            exists<'x> {
                let local: u32 = 0_u32;
                let x: &'x u32 = &'x local;
                caller::<'x>(x);
            }
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
        commit-and-verify: as strict
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// `'x == 'a` from the bound match is left to the borrow checker and
/// fails: `y` borrows a local...
#[test]
fn may_spec_local_region_bound_rejected_by_borrowck() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        impl Super for &'static u32 {}
        fn needs_super<'b>(y: &'b u32) -> () where &'b u32: Super { }
        fn spec<'x>(y: &'x u32) -> () where may_spec(&'x u32: Super) {
            if impls &'x u32: Super { needs_super::<'x>(y); } else { }
        }
        fn caller<'a>(x: &'a u32) -> () where may_spec(&'a u32: Super) {
            exists<'x> {
                let local: u32 = 0_u32;
                let y: &'x u32 = &'x local;
                spec::<'x>(y);
            }
        }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Super), via: @ may_spec(&!lt_1 u32 : Super), assumptions: {@ wf(?lt_0), @ may_spec(&!lt_1 u32 : Super)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Super), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ may_spec(&!lt_1 u32 : Super)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Super(&!lt_1 u32)`

            the rule "bound unifies" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Super(&!lt_1 u32)]
                &goal = Super(&?lt_0 u32)

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: err:
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = local : u32
                &access.place = local : u32

            the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
              condition evaluated to false: `outlived_by_loan.iter().all(|p| match p
              {
                  Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.as_ref()
                  {
                      Lt::Static => false, Lt::Variable(Variable::UniversalVar(_)) => false,
                      Lt::Variable(Variable::ExistentialVar(_)) => true,
                      Lt::Variable(Variable::BoundVar(_)) =>
                      panic!("cannot outlive a bound var"), Lt::Erased => true,
                  }, Parameter::Const(_) => panic!("cannot outlive a constant"),
              })`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `local`
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}

/// ...and holds with `y` derived from the argument.
#[test]
fn may_spec_local_region_bound_accepted() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        impl Super for &'static u32 {}
        fn needs_super<'b>(y: &'b u32) -> () where &'b u32: Super { }
        fn spec<'x>(y: &'x u32) -> () where may_spec(&'x u32: Super) {
            if impls &'x u32: Super { needs_super::<'x>(y); } else { }
        }
        fn caller<'a>(x: &'a u32) -> () where may_spec(&'a u32: Super) {
            exists<'x> {
                let y: &'x u32 = x;
                spec::<'x>(y);
            }
        }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Super), via: @ may_spec(&!lt_1 u32 : Super), assumptions: {@ wf(?lt_0), @ may_spec(&!lt_1 u32 : Super)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(&?lt_0 u32 : Super), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0), @ may_spec(&!lt_1 u32 : Super)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            the rule "bound instance" at (may_spec.rs) failed because
              pattern `Wc::ForAll(binder)` did not match value `Super(&!lt_1 u32)`

            the rule "bound unifies" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`

            the rule "same bound" at (may_spec.rs) failed because
              condition evaluated to false: `bounds.contains(&goal)`
                bounds = [Super(&!lt_1 u32)]
                &goal = Super(&?lt_0 u32)

            the rule "does not hold" at (may_spec.rs) failed because
              condition evaluated to false: `!provable_with_regions_deferred(&decls, &env, &assumptions, &goal)`

            the rule "holds" at (may_spec.rs) failed because
              condition evaluated to false: `allowed_by_mode(&decls, &env, &c)`
        commit-and-verify: ok
        bail-on-regions: as strict
        always-applicable: as strict
    "#]])
}
